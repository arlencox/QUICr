module H = DS.Hashcons

let create = H.create

module Int = struct
  type t = int
  let compare = (-)
end

module ISet = Set.Make(Int)
module IMap = Map.Make(Int)

type l =
  | Var of int
  | And of c * c
  | Or of c * c
  | Not of c
  | Exists of int * c
  | ForAll of int * c
  | True
  | False
and c = l H.hash_consed

type h = l H.t

let mvar h v =
  H.hashcons h (Var v)

let mand h a b =
  let e = match a.H.node,b.H.node with
    | True, b -> b
    | a, True -> a
    | False, _
    | _, False -> False
    | _ -> And(a,b)
  in
  H.hashcons h e

let mor h a b =
  let e = match a.H.node,b.H.node with
    | False, b -> b
    | a, False -> a
    | True, _
    | _, True -> True
    | _ -> Or(a,b)
  in
  H.hashcons h e

let mnot h a =
  let e = match a.H.node with
    | False -> True
    | True -> False
    | Not a -> a.H.node
    | _ -> Not a
  in
  H.hashcons h e

let mexists h v a =
  let e = match a.H.node with
    | True -> True
    | False -> False
    | Var v' when v == v' -> True
    | _ -> Exists(v,a)
  in
  H.hashcons h e

let mforall h v a =
  let e = match a.H.node with
    | True -> True
    | False -> False
    | Var v' when v == v' -> False
    | _ -> ForAll(v,a)
  in
  H.hashcons h e

let mtrue h = H.hashcons h True

let mfalse h = H.hashcons h False

let mimply h a b =
  mor h (mnot h a) b

let meq h a b =
  mand h (mimply h a b) (mimply h b a)

type prenex_state = {
  var_map : int IMap.t;
  frontier : int;
  clauses : int list list;
}

type qtree =
  | QTLeaf
  | QTUniversal of int * qtree
  | QTExistential of int * qtree
  | QTBranch of qtree * qtree
  | QTNot of qtree

let qtleaf = QTLeaf

let qtexists v t = QTExistential (v,t)

let qtforall v t = QTUniversal (v,t)

let qtbranch t1 t2 = QTBranch (t1,t2)

let qtnot t = QTNot t

type quantifier =
  | Existential
  | Universal

let pexists v = function
  | (Existential, l)::rest ->
    (Existential, (v::l))::rest
  | rest ->
    (Existential, [v])::rest

let pforall v = function
  | (Universal, l)::rest ->
    (Universal, (v::l))::rest
  | rest ->
    (Universal, [v])::rest

let rec qtlinearize is_neg = function
  | QTLeaf -> []
  | QTUniversal (v, rest) ->
    (if is_neg then
      pexists
    else
      pforall)
      v (qtlinearize is_neg rest)
  | QTExistential (v, rest) ->
    (if is_neg then
      pforall
    else
      pexists)
      v (qtlinearize is_neg rest)
  | QTBranch (r1, r2) ->
    (qtlinearize is_neg r1) @ (qtlinearize is_neg r2)
  | QTNot rest ->
    qtlinearize (not is_neg) rest

let qtlinearize qt =
  qtlinearize false qt

(* v = a /\ b
   a /\ b -> v ---  ~a ~b v
   v -> a      ---  ~v a
   v -> b      ---  ~v b
*)
let tseitin_and v a b cl =
  [-a; -b; v]::
  [-v; a]::
  [-v; b]::
  cl

(* v = a \/ b <-> ~v = ~a /\ ~b *)
let tseitin_or v a b cl =
  tseitin_and (-v) (-a) (-b) cl

(* v = false
   v -> false
   ~v *)
let tseitin_false v cl =
  [-v]::cl

(* v = true
   true -> v
   v *)
let tseitin_true v cl =
  [v]::cl



let prenexcnf h e =
  let vfalse = 1 in
  let vtrue = 2 in
  let frontier = 3 in
  let clauses = [[-1];[2]] in
  let visited = Hashtbl.create 8191 in
  let rec prenex state (e:c) =
    try
      Hashtbl.find visited e.H.tag
    with Not_found ->
      let quant op x a =
        let {var_map;frontier} = state in
        let xmap = try Some (IMap.find x var_map) with Not_found -> None in
        let boundx = frontier in
        let var_map = IMap.add x boundx var_map in
        let frontier = frontier + 1 in
        let ({var_map;frontier;_} as state,a,qs) = prenex {state with var_map;frontier} a in
        let var_map = match xmap with 
          | Some xmap -> IMap.add x xmap var_map
          | None -> IMap.remove x var_map
        in
        ({state with var_map;frontier}, a, op boundx qs )
      in
      let bin op a b =
        let (state,a,qs1) = prenex state a in
        let (state,b,qs2) = prenex state b in
        let freshv = state.frontier in
        let frontier = freshv + 1 in
        let clauses = op freshv a b state.clauses in
        let state = {state with frontier; clauses} in
        (state, freshv, qtbranch qs1 qs2)
      in

      let result = match e.H.node with
        | Exists (x,a) -> quant qtexists x a
        | ForAll (x,a) -> quant qtforall x a
        | Var x ->
          begin try 
              let x = IMap.find x state.var_map in
              (state, x, qtleaf)
            with Not_found ->
              let x' = state.frontier in
              let state = {
                state with
                var_map = IMap.add x x' state.var_map;
                frontier = state.frontier + 1;
              } in
              (state, x', qtleaf)
          end
        | And (a,b) -> bin tseitin_and a b
        | Or (a,b) -> bin tseitin_or a b
        | Not a ->
          let (state,a,qs) = prenex state a in
          (state, -a, qtnot qs)
        | True -> (state, vtrue, qtleaf)
        | False -> (state, vfalse, qtleaf)
      in
      Hashtbl.replace visited e.H.tag result;
      result
  in
  let (state,e,qt) = prenex {var_map = IMap.empty; frontier; clauses} e in
  (state.var_map,[e]::state.clauses,state.frontier, qtlinearize qt)

let pp_qdimacs ff (var_map, clauses, frontier, qs) =
  Printf.fprintf ff "p cnf %d %d\n" (frontier-1) (List.length clauses);
  List.iter (fun (q,vs) ->
      begin match q with
      | Universal ->
        output_string ff "a "
      | Existential ->
        output_string ff "e "
      end;
      List.iter (fun l ->
          output_string ff (string_of_int l);
          output_char ff ' '
        ) vs;
      output_string ff "0\n"
    ) qs;
  List.iter (fun cl ->
      List.iter (fun l ->
          output_string ff (string_of_int l);
          output_char ff ' '
        ) cl;
      output_string ff "0\n"
    ) clauses;
    flush ff

let rec pp_formula pp_var ff f =
  match f.H.node with
  | True -> Format.fprintf ff "true"
  | False -> Format.fprintf ff "true"
  | Var v -> Format.fprintf ff "%a" pp_var v
  | And (a,b) -> Format.fprintf ff "(%a /\\ %a)" (pp_formula pp_var) a (pp_formula pp_var) b
  | Or (a,b) -> Format.fprintf ff "(%a \\/ %a)" (pp_formula pp_var) a (pp_formula pp_var) b
  | Not a -> Format.fprintf ff "~%a" (pp_formula pp_var) a
  | Exists (v,a) -> Format.fprintf ff "(Ex %a. %a)" pp_var v (pp_formula pp_var) a
  | ForAll (v,a) -> Format.fprintf ff "(All %a. %a)" pp_var v (pp_formula pp_var) a


type 'a v = {
  mtrue: 'a;
  mfalse: 'a;
  mvar: (int -> 'a);
  mand: ('a -> 'a -> 'a);
  mor: ('a -> 'a -> 'a);
  mnot: ('a -> 'a);
  mexists: (int -> 'a -> 'a);
  mforall: (int -> 'a -> 'a);
}

let rec visit visited v (e: c) =
  try
    Hashtbl.find visited e.H.tag
  with Not_found ->
    let vrec = visit visited v in
    let result = match e.H.node with
      | True -> v.mtrue
      | False -> v.mfalse
      | Var x -> v.mvar x
      | And (a,b) ->
        let a = vrec a in
        let b = vrec b in
        v.mand a b
      | Or (a,b) ->
        let a = vrec a in
        let b = vrec b in
        v.mor a b
      | Not a ->
        let a = vrec a in
        v.mnot a
      | Exists (x,a) ->
        let a = vrec a in
        v.mexists x a
      | ForAll (x,a) ->
        let a = vrec a in
        v.mforall x a
    in
    Hashtbl.replace visited e.H.tag result;
    result


let symbols e =
  visit (Hashtbl.create 8191) {
    mtrue = ISet.empty;
    mfalse = ISet.empty;
    mvar = (fun x -> ISet.singleton x);
    mand = ISet.union;
    mor = ISet.union;
    mnot = (fun a -> a);
    mexists = (fun v a -> ISet.remove v a);
    mforall = (fun v a -> ISet.remove v a);
  } e

