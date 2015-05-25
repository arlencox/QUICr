(** Xavier Rival, May 2015
 **
 ** Importing the "set_lin" module from MemCAD as a stand-alone domain.
 ** This module is a bit experimental. We could explore splitting it into
 ** several modules, in particular using a functor representing equality
 ** relations and collapsing dimensions that are know equal (avoiding
 ** additional work in some transfer functions, but requiring some
 ** reduction work too ---not sure what side wins!)
 **
 ** This domain features a collection of basic relations:
 **  - "linear" constraints (using disjoint union)
 **  - inclusion
 **  - membership
 **  - equality
 **)

(* Todo:
 * The integration of this module here is a bit work in progress, so I leave
 * some cleaning tasks for later:
 *  - moving the libraries into utility files
 *  - now type t has a single field; remove struct
 *  - add Format printers in the maps
 *  - move stuffs specific to set_lin in a separate module
 *)

(** Maps and Sets with printers *)
module type OrderedType =
  sig
    include Set.OrderedType
    val t_2str: t -> string
  end
module type SET =
  sig
    include Set.S
    val t_2str: string -> t -> string
  end
module type MAP =
  sig
    include Map.S
    val t_2str: string -> ('a -> string) -> 'a t -> string
  end
module SetMake = functor (O: OrderedType) ->
  struct
    include Set.Make( O )
    let t_2str (sep: string) (t: t): string =
      let b = ref true in
      fold
        (fun i acc ->
          let locsep = if !b then "" else sep in
          b := false;
          Printf.sprintf "%s%s%s" acc locsep (O.t_2str i)
        ) t ""
  end
module MapMake = functor (O: OrderedType) ->
  struct
    include Map.Make( O )
    let t_2str (sep: string) (f_2str: 'a -> string) (t: 'a t): string =
      let b = ref true in
      fold
        (fun i x acc ->
          let locsep = if !b then "" else sep in
          b := false;
          Printf.sprintf "%s%s%s => %s" acc locsep (O.t_2str i) (f_2str x)
        ) t ""
  end
module IntOrd =
  struct
    type t = int
    let compare = (-)
    let t_2str = string_of_int
  end
module IntMap =
  struct
    include MapMake(IntOrd)
    let of_list l =
      List.fold_left (fun acc (k, v) -> add k v acc) empty l
  end
module IntSet = SetMake(IntOrd)

(* Printing maps and sets *)
let gen_set_2str (c: string) (f: int -> string) (s: IntSet.t): string =
  let _, str =
    (IntSet.fold
       (fun i (b, acc) ->
         false, Printf.sprintf "%s%s%s" acc (if b then "" else c) (f i)
       ) s (true, "")) in
  str
let set_setv_2str = gen_set_2str ", " (Printf.sprintf "S[%d]")
let set_sv_2str = gen_set_2str ", " (Printf.sprintf "N[%d]")



(** Module abbrevs *)

module L = LogicSymbolicSet


(** Abstract values *)

(* Linear constraints correspond to an equality of the form
 *     S = { x_0, x_1, ..., x_n } \uplus S_0 \uplus ... \uplus S_k
 *  - sl_elts is the set   x_0, x_1, ..., x_n
 *  - sl_sets is the set   S_0, S_1, ..., S_k *)
type set_lin =
    { sl_elts: IntSet.t; (* elements *)
      sl_sets: IntSet.t  (* sets *) }

(* Underlying type for constraints:
 *  - u_lin maps S to the linear constraint mentioned above, if any
 *     (we may have to authorize several constraints per variable
 *  - u_sub maps S to a set of sets S_0, ..., S_k known to be subsets of S
 *  - u_mem maps S to a set of elements x_0, ..., x_n known to belong to S
 *  - u_eqs maps S to a set of sets S_0, ..., S_k known to be equal to S *)
type u =
    { u_lin:   set_lin IntMap.t;  (* Si = a linear constraint (if any) *)
      u_sub:   IntSet.t IntMap.t; (* Si => set of subsets of Si *)
      u_mem:   IntSet.t IntMap.t; (* Si => set of elements *)
      u_eqs:   IntSet.t IntMap.t  (* Si => set of equal sets *) }

(* The type of abstract elements (bottom or non bottom) *)
type t = u option (* None if _|_, Some u if non bot constraints u *)


(** Abstract domain interface types, and "initialization" *)

type ctx = unit
type sym = int
type cnstr = sym L.t
type output = cnstr
type query = sym L.q
let init () = ()


(** Error report and debugging *)

(* Main debug switch *)
let debug_module = true


(** Lattice elements *)

(* Bottom element (empty context) *)
let bottom (): t = None
let is_bottom (x: t): bool = x = None
    
(* Top element (empty context) *)
let top (): t = Some { u_lin   = IntMap.empty;
                       u_sub   = IntMap.empty;
                       u_mem   = IntMap.empty;
                       u_eqs   = IntMap.empty }
let is_top (x: t): bool =
  match x with
  | None -> false
  | Some u ->
      u.u_lin = IntMap.empty && u.u_sub = IntMap.empty
        && u.u_sub = IntMap.empty && u.u_eqs = IntMap.empty

(* Context management *)
let context (x: t) = ( )

(* Symbols that are constrained (only those that explicitely appear) *)
let symbols (x: t): sym list =
  match x with
  | None -> [ ]
  | Some u ->
      let f = IntMap.fold (fun i s acc -> IntSet.add i (IntSet.union s acc)) in
      let acc = f u.u_sub (f u.u_mem (f u.u_eqs IntSet.empty)) in
      let acc =
        IntMap.fold
          (fun i sl acc ->
            IntSet.add i (IntSet.union (IntSet.union sl.sl_elts sl.sl_sets) acc)
          ) u.u_lin acc in
      IntSet.fold (fun i l -> i :: l) acc [ ]


(** Basic functions over set_lin *)

exception Lin_error of string (* Failure to preserve linearity *)

(* Empty (corresponds to empty set) *)
let sl_empty: set_lin =
  { sl_elts = IntSet.empty;
    sl_sets = IntSet.empty }
let sl_is_empty (sl: set_lin): bool =
  sl.sl_elts = IntSet.empty && sl.sl_sets = IntSet.empty
(* Other basic values (singleton, single set) *)
let sl_one_set (i: int) = { sl_empty with sl_sets = IntSet.singleton i }
let sl_one_elt (i: int) = { sl_empty with sl_elts = IntSet.singleton i }

(* Conversion to string *)
let sl_2str (sl: set_lin): string =
  let lin_setv_2str = gen_set_2str " + " (Printf.sprintf "S[%d]") in
  if sl.sl_elts = IntSet.empty && sl.sl_sets = IntSet.empty then
    Printf.sprintf " = empty"
  else
    Printf.sprintf " = { %s } + %s" (set_sv_2str sl.sl_elts)
      (lin_setv_2str sl.sl_sets)

(* Equality of set constraints *)
let sl_eq c sl =
  IntSet.equal c.sl_elts sl.sl_elts && IntSet.equal c.sl_sets sl.sl_sets

(* Adding two linear combinations of sets *)
let sl_add (lin0: set_lin) (lin1: set_lin): set_lin =
  if IntSet.inter lin0.sl_elts lin1.sl_elts = IntSet.empty
      && IntSet.inter lin0.sl_sets lin1.sl_sets = IntSet.empty then
    { sl_elts = IntSet.union lin0.sl_elts lin1.sl_elts;
      sl_sets = IntSet.union lin0.sl_sets lin1.sl_sets }
  else raise (Lin_error "sl_add: non disjoint constraints")
(* Inclusion of set_lin *)
let sl_subset (sl0: set_lin) (sl1: set_lin): bool =
  IntSet.subset sl0.sl_elts sl1.sl_elts
    && IntSet.subset sl0.sl_sets sl1.sl_sets
(* Subtraction of set_lin *)
let sl_sub (sl0: set_lin) (sl1: set_lin): set_lin =
  if IntSet.subset sl1.sl_elts sl0.sl_elts
      && IntSet.subset sl1.sl_sets sl0.sl_sets then
    { sl_elts = IntSet.diff sl0.sl_elts sl1.sl_elts;
      sl_sets = IntSet.diff sl0.sl_sets sl1.sl_sets }
  else raise (Lin_error "sl_sub: non disjoint constraints")

(* Linearization of an expression into a set_lin *)
let linearize (ex: int L.e): set_lin option =
  let rec aux = function
    | L.Empty -> sl_empty
    | L.Sing i -> sl_one_elt i
    | L.Var i -> sl_one_set i
    | L.DisjUnion (e0, e1) -> sl_add (aux e0) (aux e1)
    | _ -> raise (Lin_error "unsupported construction") in
  try Some (aux ex) with Lin_error _ -> None


(** Pretty-printing *)

(* Internal and abstract state representations are fairly close
 * for this domain, hence we make just one pretty-printing function *)
let pp
    (_: Format.formatter -> sym -> unit) (* sym is int... *)
    (ch: Format.formatter) (x: t): unit =
  match x with
  | None -> Format.fprintf ch "BOT\n"
  | Some u ->
      IntMap.iter (fun i c -> Format.fprintf ch "S[%d] = %s@\n" i (sl_2str c))
        u.u_lin;
      IntMap.iter
        (fun i s ->
          if s != IntSet.empty && i <= IntSet.min_elt s then
            let s = IntSet.remove i s in
            let plur = if IntSet.cardinal s > 1 then "s" else "" in
            Format.fprintf ch "S[%d] equal to set%s %s@\n" i plur
              (set_setv_2str s)
        ) u.u_eqs;
      IntMap.iter
        (fun i s ->
          let plur = if IntSet.cardinal s > 1 then "s" else "" in
          Format.fprintf ch "S[%d] contains set%s: %s\n" i plur
            (set_setv_2str s)
        ) u.u_sub;
      IntMap.iter
        (fun i s ->
          let plur = if IntSet.cardinal s > 1 then "s" else "" in
          Format.fprintf ch "S[%d] contains element%s: %s\n" i plur
            (set_sv_2str s)
        ) u.u_mem
let pp_debug = pp
let pp_print = pp
let pp_sym ch i = Format.fprintf ch "%d" i


(** Manipulating constraints *)

(* Fast access to constraints *)
let u_get_sub (u: u) (i: int): IntSet.t = (* subsets of i *)
  try IntMap.find i u.u_sub with Not_found -> IntSet.empty
let u_get_mem (u: u) (i: int): IntSet.t = (* elements of i *)
  try IntMap.find i u.u_mem with Not_found -> IntSet.empty
let u_get_eqs (u: u) (i: int): IntSet.t = (* sets equal to *)
  try IntMap.find i u.u_eqs with Not_found -> IntSet.singleton i

(* Helper functions to add basic constraints *)
let u_add_inclusion (u: u) (i: int) (j: int): u = (* i included in j *)
  let osub = u_get_sub u j in
  { u with u_sub = IntMap.add j (IntSet.add i osub) u.u_sub }
let u_add_mem (u: u) (i: int) (j: int): u = (* i is an element of j *)
  let omem = u_get_mem u j in
  { u with u_mem = IntMap.add j (IntSet.add i omem) u.u_mem }
let u_add_eq (u: u) (i: int) (j: int): u = (* i = j *)
  let c = IntSet.union (u_get_eqs u i) (u_get_eqs u j) in
  { u with u_eqs = IntSet.fold (fun i -> IntMap.add i c) c u.u_eqs }
let u_add_lin (u: u) (i: int) (sl: set_lin): u = (* i = sl *)
  (* a bit of reduction: search of all other equalities to sl *)
  let u =
    IntMap.fold
      (fun j0 sl0 acc ->
        if sl_eq sl sl0 then u_add_eq u i j0
        else u
      ) u.u_lin u in
  if IntMap.mem i u.u_lin then
    Printf.printf "WARN,u_add_lin: over-writing constraint";
  { u with u_lin = IntMap.add i sl u.u_lin }

(* Guard operator, adds a new constraint *)
let constrain (c: cnstr) (x: t): t =
  match x with
  | None -> x
  | Some u ->
      let u =
        (* Basic constraints added using the utility functions:
         *  all constraints needed in the graph examples are supported,
         *  except the S0 = S1 \cup S2 (not \uplus), as I believe such
         *  constraints should just not arise here! *)
        match c with
        | L.In (i, L.Var j) ->
            u_add_mem u i j
        | L.Eq (L.Var i, L.Empty) | L.Eq (L.Empty, L.Var i) ->
            if IntMap.mem i u.u_lin then
              Printf.printf "WARN,constrain: existing linear constraint";
            let cons = { sl_elts = IntSet.empty; sl_sets = IntSet.empty } in
            { u with u_lin = IntMap.add i cons u.u_lin }
        | L.Eq (L.Var i, L.Var j) ->
            u_add_eq u i j
        | L.Eq (L.Var i, L.DisjUnion (L.Var j, L.Var k))
        | L.Eq (L.DisjUnion (L.Var j, L.Var k), L.Var i) ->
            u_add_lin u i { sl_sets = IntSet.add j (IntSet.singleton k);
                            sl_elts = IntSet.empty }
        | L.Eq (L.Var i, L.Sing j) | L.Eq (L.Sing j, L.Var i)
        | L.Eq (L.Var i, L.DisjUnion (L.Empty, L.Sing j))
        | L.Eq (L.Var i, L.DisjUnion (L.Sing j, L.Empty)) ->
            u_add_lin u i { sl_sets = IntSet.empty;
                            sl_elts = IntSet.singleton j }
        | L.Eq (L.Var i, L.DisjUnion (L.Sing j, L.Var k)) ->
            u_add_lin u i { sl_sets = IntSet.singleton k;
                            sl_elts = IntSet.singleton j }
        | L.Eq (L.Var i, e) ->
            begin
              match linearize e with
              | Some lin -> u_add_lin u i lin
              | None ->
                  Printf.printf "WARN,constrain: linearization failed";
                  u
            end
        | L.SubEq (L.Var i, L.Var j) ->
            (* TODO: there seem to be a bug in the inclusion order,
             *   that is, constraints seem to appear in the reverse
             *   order than they should *)
            (*if debug_module then
              Printf.printf "inclusion: %d <= %d\n" i j;*)
            u_add_inclusion u i j
        | _ ->
            (* otherwise, we just drop the constraint *)
            Format.eprintf "WARN,constrain,ignored: %a" (L.pp pp_sym) c;
            u in
      Some u

(* Helper functions to check basic constraints *)
exception Stop of bool
(* a closure function: inputs Si, and returns all Sj found to be
 *  contained in Si, using equality and inclusion constraints *)
let closure_subsets (u: u) (i: int): IntSet.t =
  let add acc s =
    IntSet.fold
      (fun i (nvs, acc) ->
        if IntSet.mem i acc then nvs, acc
        else i :: nvs, IntSet.add i acc
      ) s ([ ], acc) in
  let rec aux acc s =
    let nvs, acc = add acc s in
    if nvs = [ ] then acc
    else
      let acc, next =
        List.fold_left
          (fun (old, n) j ->
            IntSet.add j old,
            IntSet.union (u_get_eqs u j) (IntSet.union (u_get_sub u j) n)
          ) (acc, IntSet.empty) nvs in
      aux acc next in
  aux IntSet.empty (IntSet.singleton i)
(* does u entail that i \in j ? *)
let u_sat_mem (u: u) i j =
  try
    if IntSet.mem i (u_get_mem u j) then raise (Stop true);
    (* look at sets equal to j, and gather their known elements *)
    let smems = closure_subsets u j in
    let elems =
      IntSet.fold
        (fun i acc -> IntSet.union (u_get_mem u i) acc) smems IntSet.empty in
    if IntSet.mem i elems then raise (Stop true);
    if IntSet.mem i (IntMap.find j u.u_lin).sl_elts then raise (Stop true);
    false
  with
  | Stop b -> b
  | Not_found -> false
(* does u entail i <= j ? (i subset of j) *)
let u_sat_sub (u: u) i j =
  let i0 = u_get_eqs u i and j0 = u_get_eqs u j in
  let jsubsets =
    IntSet.fold (fun jj acc -> IntSet.union acc (u_get_sub u jj)) j0
      IntSet.empty in
  IntSet.inter i0 jsubsets != IntSet.empty
(* does u entail that i in ex *)
let u_sat_mem_ex (u: u) i ex =
  let rec aux_mem = function
    | L.Empty -> false
    | L.Sing j -> i = j
    | L.Var j -> u_sat_mem u i j
    | L.DisjUnion (e0, e1) | L.Union (e0, e1) -> aux_mem e0 || aux_mem e1
    | _ -> false in
  if aux_mem ex then true
  else
    match linearize ex with
    | Some lin ->
        (* we look for sets that include this linearized expression,
         * and try to verify membership on them *)
        begin
          try
            IntMap.iter
              (fun j sl ->
                if sl_subset lin sl && u_sat_mem u i j then raise (Stop true)
              ) u.u_lin;
            false
          with Stop b -> b
        end
    | None -> false (* we give up *)
(* can we find a constraint that ensures i is empty ? *)
let u_has_empty_ctr (u: u) (i: int): bool =
  try
    let sl = IntMap.find i u.u_lin in
    sl.sl_elts = IntSet.empty && sl.sl_sets = IntSet.empty
  with Not_found -> false
(* is i empty ? *)
let u_sat_empty (u: u) i =
  IntSet.exists (fun i -> u_has_empty_ctr u i) (u_get_eqs u i)
(* does u entail i=j ? *)
let u_sat_eq (u: u) i j =
  IntSet.mem i (u_get_eqs u j) || IntSet.mem j (u_get_eqs u i)
(* does constraint "i=sl" exist in u ? *)
let u_sat_lin (u: u) i sl =
  try (* NB: precision can be improved by looking at set equalities *)
    let c = IntMap.find i u.u_lin in
    sl_eq c sl
  with Not_found ->
    if IntSet.is_empty sl.sl_elts then
      let sl_sets =
        IntSet.fold
          (fun i acc ->
            if u_sat_empty u i then acc
            else IntSet.add i acc
          ) sl.sl_sets IntSet.empty in
      if IntSet.cardinal sl_sets = 1 then
        u_sat_eq u (IntSet.choose sl_sets) i
      else false
    else false

(* Sat operator, checks whether a constraint is implied by an abstract state *)
let sat (x: t) (c: cnstr): bool =
  match x with
  | None -> true (* any constraint valid under _|_ *)
  | Some u ->
      (* trying to support the same constraints as in guard *)
      match c with
      | L.In (i, ex) -> u_sat_mem_ex u i ex
      | L.Eq (L.Var i, L.Var j) -> u_sat_eq u i j
      | L.SubEq (L.Var i, L.Var j) -> u_sat_sub u i j
      | L.Eq (L.Var i, ex) | L.Eq (ex, L.Var i) ->
          begin
            match linearize ex with
              | None -> false
              | Some lin -> u_sat_lin u i lin
          end
      | _ -> false

let serialize (x: t): output =
  Printf.printf "WARN: serialize will return default, imprecise result\n";
  L.True


(** Manipulating symbols *)
let forget _ = failwith "forget"
let rename_symbols _ = failwith "rename_symbols"


(** Lattice binary operations *)
let join _ = failwith "join"
let widening _ = failwith "widening"
let meet _ = failwith "meet"

(* Inclusion checking *)
let le (x0: t) (x1: t): bool =
  let module M = struct exception Abort end in
  (* are all constraints of u1 true in u0 ? *)
  let u_is_le (u0: u) (u1: u): bool =
    try
      IntMap.iter
        (fun i sl1 ->
          if not (sl_eq sl1 (IntMap.find i u0.u_lin)) then raise M.Abort
        ) u1.u_lin;
      IntMap.iter
        (fun i elts1 ->
          if not (IntSet.subset elts1 (u_get_mem u0 i)) then raise M.Abort
        ) u1.u_mem;
      IntMap.iter
        (fun i sub1 ->
          if not (IntSet.subset sub1 (u_get_sub u0 i)) then raise M.Abort
        ) u1.u_sub;
      IntMap.iter
        (fun i eqs1 ->
          if not (IntSet.subset eqs1 (u_get_eqs u0 i)) then raise M.Abort
        ) u1.u_eqs;
      true
    with
    | Not_found -> false
    | M.Abort -> false in
  match x0, x1 with
  | None, _ -> true
  | Some _, None -> false
  | Some u0, Some u1 -> u_is_le u0 u1


(** Interface for reduction *)

(* For now, this domain still offers no facility for reduction *)
let query (x: t): query =
  Printf.printf "WARN: query will return default, imprecise result\n";
  { L.get_eqs     = (fun ( ) -> [ ]);
    L.get_eqs_sym = (fun _ -> [ ]); }
let combine (q: query) (x: t) =
  Printf.printf "WARN: combine will return default, imprecise result\n";
  x


(* From now on, code being ported from MemCAD

open Data_structures
open Lib

open Nd_sig
open Set_sig
open Svenv_sig

open Nd_utils
open Set_utils


(** Error handling *)
let this_module = "set-lin"
let error = gen_error this_module
let todo  = gen_todo  this_module
let warn  = gen_warn  this_module


let debug_module = true

type sv = int

module Set_lin =
  (struct
    (** Some basic functions over set_lin *)
    (* Just a setv *)
    let sl_setv (setv: int): set_lin =
      { sl_elts = IntSet.empty;
        sl_sets = IntSet.singleton setv }
    (* Selection of the most general set_lin: (1) fewer elts, (2) fewer sets *)
    let sl_more_gen (sl0: set_lin) (sl1: set_lin): set_lin =
      let default ( ) = warn "bad case"; sl1 in
      let ce0 = IntSet.cardinal sl0.sl_elts
      and ce1 = IntSet.cardinal sl1.sl_elts in
      if ce0 < ce1 then sl0
      else if ce0 > ce1 then sl1
      else
        let cs0 = IntSet.cardinal sl0.sl_sets
        and cs1 = IntSet.cardinal sl1.sl_sets in
        if cs0 < cs1 then sl0
        else if cs0 > 0 then
          let m0 = IntSet.max_elt sl0.sl_sets
          and m1 = IntSet.max_elt sl1.sl_sets in
          if m0 > m1 then sl0
          else if m0 < m1 then sl1
          else default ( )
        else default ( )


    (** General utility functions *)

    (* map over the option type *)
    let t_map (f: u -> u) (t: t): t =
      match t.t_t with
      | None -> t
      | Some u -> { t with t_t = Some (f u) }

    (* Empty element *)
    let empty (uo: u option): t = (* corresponds to top in fact *)
      { t_t     = uo;
        t_roots = IntSet.empty; }

    (* Remove all constraints over a group of svs in one shot;
     * function f maps a symbolic variable to true if it should be dropped,
     * and to false otherwise. *)
    let drop_symb_svs (f: int -> bool) (x: t): t =
      match x.t_t with
      | None -> x
      | Some u ->
          let lin =
            IntMap.fold
              (fun i c acc ->
                let b = IntSet.fold (fun j acc -> acc || f j) c.sl_elts false in
                if b then IntMap.remove i acc else acc
              ) u.u_lin u.u_lin in
          let mem =
            IntMap.fold
              (fun i c acc ->
                let b = IntSet.fold (fun j acc -> acc || f j) c false in
                if b then IntMap.remove i acc else acc
              ) u.u_mem u.u_mem in
          { x with t_t = Some { u with u_lin = lin; u_mem = mem } }

    (* Remove all constraints over a group of setvs in one shot;
     * function fv maps a setv to true if it should be dropped, and to false
     * otherwise *)
    let drop_setvs (fv: int -> bool) (x: t): t =
      let fset s = IntSet.fold (fun i acc -> acc || fv i) s false in
      let filter_set s =
        if fset s then
          IntSet.fold
            (fun i acc -> if fv i then acc else IntSet.add i acc) s IntSet.empty
        else s in
      match x.t_t with
      | None -> x
      | Some u ->
          let lin, rem =
            IntMap.fold
              (fun i c (acc, rem) ->
                if fv i || fset c.sl_sets then IntMap.remove i acc, (i,c) :: rem
                else acc, rem
              ) u.u_lin (u.u_lin, [ ]) in
          let lin =
            if List.length rem > 1 then
              let f (i,c) =
                (i,c), IntSet.cardinal c.sl_elts + IntSet.cardinal c.sl_sets in
              let rem = List.map f rem in
              let rem = List.sort (fun (_,i) (_,j) -> i - j) rem in
              let rem = List.map fst rem in
              match rem with
              | (i0, sl0) :: (i1, sl1) :: _ ->
                  (* tries to save a constraint *)
                  if sl_subset sl0 sl1 then
                    let sl = sl_add (sl_sub sl1 sl0) (sl_setv i0) in
                    Printf.printf "producing: %d :> %s\n" i1 (sl_2str sl);
                    IntMap.add i1 sl lin
                  else lin
              | _ -> lin
            else lin in
          let sub =
            IntMap.fold
              (fun i sub acc ->
                if fv i then acc
                else IntMap.add i (filter_set sub) acc
              ) u.u_sub IntMap.empty in
          let eqs =
            IntMap.fold
              (fun i s acc ->
                if fv i then IntMap.remove i acc
                else if fset s then
                  let fs = filter_set s in
                  if IntSet.cardinal fs = 1 && IntSet.mem i fs then
                    IntMap.remove i acc
                  else IntMap.add i fs acc
                else acc
              ) u.u_eqs u.u_eqs in
          let mem =
            IntMap.fold
              (fun i _ acc ->
                if fv i then IntMap.remove i acc
                else acc
              ) u.u_mem u.u_mem in
          { x with t_t = Some { u_lin = lin;
                                u_sub = sub;
                                u_mem = mem;
                                u_eqs = eqs } }


    (** A bit of reduction, for internal use only *)
    (* Notes:
     *  - we would need less reduction with an equality functor
     *  - we could share code with iterators
     *)
    (* Reduction of the linear part (replace a variable by lin expr if any) *)
    let t_reduce_lin (setv: int) (t: t): t =
      let u_aux (u: u): u =
        if IntMap.mem setv u.u_lin then
          let lin0 = IntMap.find setv u.u_lin in
          let nlins =
            IntMap.fold
              (fun i lin acc ->
                if IntSet.mem setv lin.sl_sets then (* do the reduction *)
                  let lin =
                    sl_add lin0
                      { lin with sl_sets = IntSet.remove setv lin.sl_sets } in
                  IntMap.add i lin acc
                else (* nothing changes *) acc
              ) u.u_lin u.u_lin in
          { u with u_lin = nlins }
        else u in
      t_map u_aux t
    (* Replacing a setv by an equal setv if there is one *)
    let t_reduce_eq (setv: int) (t: t): t =
      let u_aux (u: u): u =
        if IntMap.mem setv u.u_eqs then
          let others = IntSet.remove setv (IntMap.find setv u.u_eqs) in
          if IntSet.cardinal others > 0 then
            let setv0 = IntSet.min_elt others in
            (* replace setv by setv0 everywhere if possible *)
            let nlins =
              IntMap.fold
                (fun i l acc ->
                  if IntSet.mem setv l.sl_sets then
                    let s = IntSet.add setv0 (IntSet.remove setv l.sl_sets) in
                    let nl = { l with sl_sets = s } in
                    IntMap.add i nl acc
                  else acc
                ) u.u_lin u.u_lin in
            let nsubs =
              IntMap.fold
                (fun i s acc ->
                  if i = setv then
                    let s = IntSet.union s (u_get_sub u setv0) in
                    IntMap.add setv s acc
                  else if IntSet.mem setv s then
                    let s = IntSet.add setv0 (IntSet.remove setv s) in
                    IntMap.add setv s acc
                  else acc
                ) u.u_sub u.u_sub in
            let nmems =
              if IntMap.mem setv u.u_mem then
                let s = IntMap.find setv u.u_mem in
                let s0 = u_get_mem u setv0 in
                IntMap.add setv0 (IntSet.union s s0) u.u_mem
              else u.u_mem in
            let nlins =
              if IntMap.mem setv nlins then
                if IntMap.mem setv0 nlins then
                  let sl0 = IntMap.find setv0 nlins
                  and sl  = IntMap.find setv  nlins in
                  warn "choice between two set_lin equalities";
                  IntMap.add setv0 (sl_more_gen sl sl0) nlins
                else IntMap.add setv0 (IntMap.find setv nlins) nlins
              else nlins in
            { u with
              u_lin = nlins;
              u_sub = nsubs;
              u_mem = nmems; }
          else u
        else u in
      t_map u_aux t


    (** Management of symbolic variables *)

    (* For sanity check *)
    let check_nodes (s: IntSet.t) (x: t): bool = todo "check_nodes"

    (* Symbolic variables *)
    let sv_add (sv: sv) (t: t): int * t = sv, t
    let sv_rem (sv: sv) (t: t): t =
      drop_symb_svs ((=) sv) t

    (* check if a set var root *)
    let is_setv_root (setv: sv) (t: t): bool =
      IntSet.mem setv t.t_roots

    (* collect root set variables *)
    let setv_col_root (t: t): IntSet.t = t.t_roots

    (* Set variables *)
    let setv_add ?(root: bool = false) ?(kind: set_kind option = None) 
        ?(name: string option = None) (setv: int) (t: t): t =
      { t with
        t_roots = if root then IntSet.add setv t.t_roots else t.t_roots }
    let setv_rem (setv: int) (t: t): t =
      let loc_debug = debug_module in
      if loc_debug then
        Printf.printf "removing setv S[%d] in\n%s\n" setv (t_2stri "  " t);
      let t = t_reduce_lin setv t in
      let t = t_reduce_eq setv t in
      if loc_debug then
        Printf.printf "reduction done:\n%s\n" (t_2stri "  " t);
      let t = drop_setvs ((=) setv) t in
      let t = { t with t_roots = IntSet.remove setv t.t_roots } in
      if loc_debug then
        Printf.printf "removed setv S[%d]:\n%s\n" setv (t_2stri "  " t);
      t


    (** Comparison and join operators *)


    (* Lower upper bound *)
    let u_lub (u0: u) (u1: u): u =
      (* A useful generalization:
       *   (S0 = S1 /\ S2 = empty) \/ (S0 = {x} + S1 /\ S2 = {x})
       *           = (S0 = S2 + S1)
       * To do it, we first compute singletons in both sides
       * Then, we compute lists of generalized constraints
       *
       * List of equalities that could be added *)
      let generalize_sngs (u: u): (int * set_lin) list =
        (* singletons: set => { elt } *)
        let compute_singletons (u: u): (int, int) Bi_fun.t * set_lin IntMap.t =
          IntMap.fold
            (fun i sl (acc, rem) ->
              if sl.sl_sets = IntSet.empty
                  && IntSet.cardinal sl.sl_elts = 1 then
                Bi_fun.add i (IntSet.min_elt sl.sl_elts) acc,
                IntMap.remove i rem
              else acc, rem
            ) u.u_lin (Bi_fun.empty, u.u_lin) in
        let sngs, rem = compute_singletons u in
        let l =
          IntMap.fold
            (fun i sl acc ->
              if IntSet.cardinal sl.sl_elts = 1 then
                let x = IntSet.min_elt sl.sl_elts in
                match Bi_fun.inverse_opt x sngs with
                | None -> acc
                | Some j -> (i, { sl_elts = IntSet.remove x sl.sl_elts;
                                  sl_sets = IntSet.add j sl.sl_sets; } ) :: acc
              else acc
            ) rem [ ] in
        if debug_module then
          Printf.printf "reduced constraints: %d\n" (List.length l);
        l in
      let compute_emptys (u: u) =
        IntMap.fold
          (fun i sl acc -> if sl_is_empty sl then IntSet.add i acc else acc)
          u.u_lin IntSet.empty in
      let gen0 = generalize_sngs u0 and gen1 = generalize_sngs u1 in
      let emp0 = compute_emptys u0 and emp1 = compute_emptys u1 in
      (* generalized equalities of set a, emptys of set b, element of side b *)
      let generalize (gena: (int * set_lin) list) (empb: IntSet.t) (ub: u) =
        List.fold_left
          (fun acc (i, sl) ->
            if sl.sl_elts = IntSet.empty then
              let rec_sets =
                IntSet.fold
                  (fun i acc ->
                    if IntSet.mem i empb then IntSet.remove i acc else acc
                  ) sl.sl_sets sl.sl_sets in
              if IntSet.cardinal rec_sets = 1 then
                let x = IntSet.min_elt rec_sets in
                (* means we have equality x = i *)
                let b = u_sat_eq ub i x in
                if debug_module then
                  Printf.printf "suggests equality S%d = S%d => %b\n" i x b;
                (i, sl) :: acc
              else acc
            else acc
          ) [ ] gena in
      let lineqs_to_add = generalize gen0 emp1 u1 @ generalize gen1 emp0 u0 in
      (* TODO: we may want to do some more reduction before we run join *)
      let lin =
        IntMap.fold
          (fun i sl0 acc ->
            try
              let sl1 = IntMap.find i u1.u_lin in
              if sl_eq sl0 sl1 then IntMap.add i sl0 acc else acc
            with Not_found -> acc
          ) u0.u_lin IntMap.empty in
      let lin =
        List.fold_left
          (fun acc (i, sl) ->
            if IntMap.mem i acc then error "already here!"
            else IntMap.add i sl acc
          ) lin lineqs_to_add in
      (* XR: small local precision improvement;
       *   - it solves the graph-16 problem with set domain precision
       *     (yet, then, graph-16 fails later, in unfolding, and I could
       *      still not check why)
       *   - but it acutally also breaks graph-02
       *     This is bad, but it is a good example for non monoticity of
       *     static analysis: you improve precision somewhere, and it gets
       *     worse somewhere else... Tough life... :-)
       *     I will need to investigate! *)
      let lin =
        IntMap.fold
          (fun i sl acc ->
            if not (IntMap.mem i lin) && u_sat_lin u0 i sl then
              IntMap.add i sl acc
            else acc
          ) u1.u_lin lin in
      let eqs =
        IntMap.fold
          (fun i eqs0 acc ->
            let eqs = IntSet.inter eqs0 (u_get_eqs u1 i) in
            if IntSet.cardinal eqs <= 1 then acc (* only i ! *)
            else IntMap.add i eqs acc
          ) u0.u_eqs IntMap.empty in
      let mem =
        IntMap.fold
          (fun i mem0 acc ->
            let mem = IntSet.inter mem0 (u_get_mem u1 i) in
            if mem = IntSet.empty then acc
            else IntMap.add i mem acc
          ) u0.u_mem IntMap.empty in
      let sub =
        IntMap.fold
          (fun i sub0 acc ->
            let sub = IntSet.inter sub0 (u_get_sub u1 i) in
            if sub = IntSet.empty then acc
            else IntMap.add i sub acc
          ) u0.u_sub IntMap.empty in
      { u_lin = lin;
        u_eqs = eqs;
        u_mem = mem;
        u_sub = sub }
    let t_lub (t0: t) (t1: t): t =
      (* TODO: we should check consistency of t_roots ! *)
      match t0.t_t, t1.t_t with
      | None, _ -> t1
      | _, None -> t0
      | Some u0, Some u1 -> { t0 with t_t = Some (u_lub u0 u1) }

    (* Weak bound: serves as widening *)
    let weak_bnd (t0: t) (t1: t): t = t_lub t0 t1
    (* Upper bound: serves as join and widening *)
    let upper_bnd (t0: t) (t1: t): t = t_lub t0 t1


    (** Forget (if the meaning of the sv changes) *)
    let forget (sv: int) (t: t): t = (* will be used for assign *)
      let f i = i = sv in
      drop_symb_svs f t


    (** Renaming (e.g., post join) *)
    (* Xavier:
     *  In all other domains, this function takes a single abstract element,
     *  that has not been renamed yet, and fully renames variables in it
     *  (doing some duplications if that is required).
     *  It is usually called once after a shape join, and before a "numeric"
     *  join. I could not understand the way it is called in dom_mem_low_list.
     *  The implementation below renames t1, and basically ignores t2 (after
     *  issuing a warning if t2 contains any constraint, i.e., is not top).
     *)
    (* TODO: share code ! *)
    let symvars_srename_setv_mapping (* for is_le *)
        (om: (Offs.t * int) Offs.OffMap.t)
        (nm: (int * Offs.t) node_mapping)
        (svm: setv_mapping) (t1: t): t =
      if debug_module then
        Printf.printf "begin:\n%s" (t_2stri "  " t1);
      assert (om = Offs.OffMap.empty);
      let t1 =
        drop_symb_svs (fun i -> IntSet.mem i nm.nm_rem)
          (drop_setvs (fun i -> IntSet.mem i svm.sm_rem) t1) in
      let do_sv (i: int): int =
        try fst (IntMap.find i nm.nm_map)
        with Not_found -> (*i*) error "do_sv fails" in
      let do_sv_a (i: int): IntSet.t =
        try let x, y = IntMap.find i nm.nm_map in IntSet.add x y
        with Not_found -> (*IntSet.singleton i*) error "do_sv_a fails" in
      let do_setv (i: int): int =
        try fst (IntMap.find i svm.sm_map)
        with Not_found ->
          warn (Printf.sprintf "do_setv fails: %d" i);
          i in
      let do_sv_set (s: IntSet.t): IntSet.t =
        IntSet.fold (fun i acc -> IntSet.add (do_sv i) acc) s IntSet.empty in
      let do_sv_set_a (s: IntSet.t): IntSet.t =
        IntSet.fold
          (fun i acc -> IntSet.union (do_sv_a i) acc) s IntSet.empty in
      let do_setv_set (s: IntSet.t): IntSet.t =
        IntSet.fold (fun i acc -> IntSet.add (do_setv i) acc) s IntSet.empty in
      let do_set_lin (sl: set_lin): set_lin =
        { sl_elts  =  do_sv_set sl.sl_elts;
          sl_sets  =  do_setv_set sl.sl_sets } in
      let do_u (u: u): u =
        let lin =
          IntMap.fold (fun setv sl -> IntMap.add (do_setv setv) (do_set_lin sl))
            u.u_lin IntMap.empty in
        let sub =
          IntMap.fold (fun setv s -> IntMap.add (do_setv setv) (do_setv_set s))
            u.u_sub IntMap.empty in
        let mem =
          IntMap.fold (fun setv s -> IntMap.add (do_setv setv) (do_sv_set s))
            u.u_mem IntMap.empty in
        let eqs =
          IntMap.fold (fun setv s -> IntMap.add (do_setv setv) (do_setv_set s))
            u.u_eqs IntMap.empty in
        let u =
          { u_lin  = lin;
            u_mem  = mem;
            u_sub  = sub;
            u_eqs  = eqs } in
        IntMap.fold (* adding equalities from the mapping *)
          (fun _ (setv0, s) acc ->
            IntSet.fold (fun j acc -> u_add_eq acc setv0 j) s acc
          ) svm.sm_map u in
      let u =
        match t1.t_t with
        | None -> None
        | Some u ->
            if debug_module then
              Printf.printf
                "argument:\n%snode mapping:\n%sset mapping:\n%s"
                (t_2stri "  " t1) (node_mapping_2str nm)
                (setv_mapping_2str svm);
            Some (do_u u) in
      (* Add set equalities *)
      let tr =
        { t_t     = u;
          t_roots = do_setv_set t1.t_roots; } in
      if debug_module then
        Printf.printf "renaming result:\n%s\n" (t_2stri "   " tr);
      tr
    let symvars_srename_no_setv_mapping (* for join *)
        (om: (Offs.t * int) Offs.OffMap.t)
        (nm: (int * Offs.t) node_mapping)
        (t1: t): t =
      if debug_module then
        Printf.printf "begin:\n%s" (t_2stri "  " t1);
      assert (om = Offs.OffMap.empty);
      let t1 = drop_symb_svs (fun i -> IntSet.mem i nm.nm_rem) t1 in
      let do_sv (i: int): int =
        try fst (IntMap.find i nm.nm_map)
        with Not_found -> (*i*) error "do_sv fails" in
      let do_sv_a (i: int): IntSet.t =
        try let x, y = IntMap.find i nm.nm_map in IntSet.add x y
        with Not_found -> (*IntSet.singleton i*) error "do_sv_a fails" in
      let do_sv_set (s: IntSet.t): IntSet.t =
        IntSet.fold (fun i acc -> IntSet.add (do_sv i) acc) s IntSet.empty in
      let do_sv_set_a (s: IntSet.t): IntSet.t =
        IntSet.fold
          (fun i acc -> IntSet.union (do_sv_a i) acc) s IntSet.empty in
      let do_set_lin (sl: set_lin): set_lin =
        { sl with sl_elts  =  do_sv_set sl.sl_elts } in
      let do_u (u: u): u =
        let lin =
          IntMap.fold (fun setv sl -> IntMap.add setv (do_set_lin sl))
            u.u_lin IntMap.empty in
        let sub = u.u_sub in
        let mem =
          IntMap.fold (fun setv s -> IntMap.add setv (do_sv_set s))
            u.u_mem IntMap.empty in
        let eqs = u.u_eqs in
        { u_lin  = lin;
          u_mem  = mem;
          u_sub  = sub;
          u_eqs  = eqs } in
      let u =
        match t1.t_t with
        | None -> None
        | Some u ->
            if debug_module then
              Printf.printf
                "argument:\n%snode mapping:\n%s"
                (t_2stri "  " t1) (node_mapping_2str nm);
            Some (do_u u) in
      (* Add set equalities *)
      let tr =
        { t_t     = u;
          t_roots = t1.t_roots; } in
      if debug_module then
        Printf.printf "renaming result:\n%s\n" (t_2stri "   " tr);
      tr
    let symvars_srename
        (om: (Offs.t * int) Offs.OffMap.t)
        (nm: (int * Offs.t) node_mapping)
        (svm: setv_mapping option) (t1: t): t =
      match svm with
      | None -> symvars_srename_no_setv_mapping om nm t1
      | Some svm -> symvars_srename_setv_mapping om nm svm t1

    (* Synchronization of the SV environment*)
    let sve_sync_top_down (sve: svenv_mod) (t: t): t =
      (* do nothing for add; remove constraints over mod and rem *)
      let f i = PSet.mem i sve.svm_rem || PSet.mem i sve.svm_mod in
      drop_symb_svs f t

    (* Removes all symbolic vars that are not in a given set *)
    let symvars_filter (skeep: IntSet.t) (setvkeep: IntSet.t) (t: t): t =
      (* Removals are done by the generic removal functions *)
      drop_symb_svs (fun i -> not (IntSet.mem i skeep))
        (drop_setvs (fun i -> not (IntSet.mem i setvkeep)) t)
  end: DOMSET)
*)
