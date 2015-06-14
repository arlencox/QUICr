module H = DS.Hashcons

let create = H.create

module Int = struct
  type t = int
  let compare = (-)
end

module ISet = Set.Make(Int)
module IMap = Map.Make(Int)

type nop =
  | And
  | Or

let nnop = function
  | And -> Or
  | Or -> And

type qop =
  | Forall
  | Exists

let nqop = function
  | Forall -> Exists
  | Exists -> Forall

type var = int

type l =
  | Var of int
  | Nop of nop * c list
  | Not of c
  | Qop of qop * var list * c
and c = l H.hash_consed

type h = l H.t

let mk_var h v : c =
  H.hashcons h (Var v)

let mk_true h : c =
  H.hashcons h (Nop (And, []))

let mk_false h : c =
  H.hashcons h (Nop (Or, []))

let mk_and h a b =
  let e = match a.H.node,b.H.node with
    | Nop (Or, []), _
    | _, Nop (Or, []) -> Nop (Or, [])
    | Nop (And, aa), Nop (And, ba) -> Nop (And, List.rev_append aa ba)
    | Nop (And, aa), _ -> Nop (And, b::aa)
    | _, Nop (And, ba) -> Nop (And, a::ba)
    | _ -> Nop (And, [a;b])
  in
  H.hashcons h e

let rec mk_andl h = function
  | [] -> mk_true h
  | hd::tl -> mk_and h hd (mk_andl h tl)


let mk_or h a b =
  let e = match a.H.node,b.H.node with
    | Nop (And, []), _
    | _, Nop (And, []) -> Nop (And, [])
    | Nop (Or, aa), Nop (Or, ba) -> Nop (Or, List.rev_append aa ba)
    | Nop (Or, aa), _ -> Nop (Or, b::aa)
    | _, Nop (Or, ba) -> Nop (Or, a::ba)
    | _ -> Nop (Or, [a;b])
  in
  H.hashcons h e

let rec mk_orl h = function
  | [] -> mk_false h
  | hd::tl -> mk_or h hd (mk_orl h tl)

let mk_not h a =
  let e = match a.H.node with
    | Nop (op, []) -> Nop (nnop op, [])
    | Not a -> a.H.node
    | _ -> Not a
  in
  H.hashcons h e

let mk_exists h v a =
  let e = match a.H.node with
    | Qop(Exists, vs, a) -> Qop(Exists, v::vs, a)
    | Nop(op, []) -> Nop(op, [])
    | _ -> Qop(Exists, [v], a)
  in
  H.hashcons h e

let mk_forall h v a =
  let e = match a.H.node with
    | Qop(Forall, vs, a) -> Qop(Forall, v::vs, a)
    | Nop(op, []) -> Nop(op, [])
    | _ -> Qop(Forall, [v], a)
  in
  H.hashcons h e

let mk_imply h a b =
  mk_or h (mk_not h a) b

let mk_eq h a b =
  mk_and h (mk_imply h a b) (mk_imply h b a)


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
    (Existential, ISet.add v l)::rest
  | rest ->
    (Existential, ISet.singleton v)::rest

let pforall v = function
  | (Universal, l)::rest ->
    (Universal, ISet.add v l)::rest
  | rest ->
    (Universal, ISet.singleton v)::rest

let rec qappend l1 l2 =
  match l1 with
  | [Universal, ul1] ->
    begin match l2 with
      | (Universal, ul2)::rest ->
        (Universal, (ISet.union ul1 ul2))::rest
      | rest -> (Universal, ul1)::rest
    end
  | [Existential, el1] ->
    begin match l2 with
      | (Existential, el2)::rest ->
        (Existential, (ISet.union el1 el2))::rest
      | rest -> (Existential, el1)::rest
    end
  | [] ->
    l2
  | h::rest ->
    h::(qappend rest l2)

let rec qtlinearize qs vs is_neg = function
  | QTLeaf -> (qs,vs)
  | QTUniversal (v, rest) ->
    let (qs, vs) = qtlinearize qs vs is_neg rest in
    if ISet.mem v vs then
      (qs, vs)
    else
      let qs' = (if is_neg then pexists else pforall) v qs in
      (qs', ISet.add v vs)
  | QTExistential (v, rest) ->
    let (qs, vs) = qtlinearize qs vs is_neg rest in
    if ISet.mem v vs then
      (qs, vs)
    else
      let qs' = (if is_neg then pforall else pexists) v qs in
      (qs', ISet.add v vs)
  | QTBranch (r1, r2) ->
    let (qs,vs) = qtlinearize qs vs is_neg r2 in
    qtlinearize qs vs is_neg r1
  | QTNot rest ->
    qtlinearize qs vs (not is_neg) rest

let qtlinearize qt =
  fst (qtlinearize [] ISet.empty false qt)


let pp_qdimacs ff (var_map, clauses, frontier, qs) =
  Printf.fprintf ff "p cnf %d %d\n" (frontier-1) (List.length clauses);
  List.iter (fun (q,vs) ->
      begin match q with
      | Universal ->
        output_string ff "a "
      | Existential ->
        output_string ff "e "
      end;
      ISet.iter (fun l ->
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
  entry: (c -> unit);
}

let rec visit visited v (e: c) =
  try
    Hashtbl.find visited e.H.tag
  with Not_found ->
    let vrec = visit visited v in
    v.entry e;
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
    entry = (fun _ -> ());
  } e

let pp_smtlib ff f =
  let s = visit (Hashtbl.create 8191) {
    mtrue = "true";
    mfalse = "false";
    mvar = (fun v -> "v"^(string_of_int v));
    mand = (fun a b -> String.concat " " ["(and"; a; b; ")"]);
    mor = (fun a b -> String.concat " " ["(or"; a; b; ")"]);
    mnot = (fun a -> String.concat " " ["(not"; a; ")"]);
    mexists = (fun v a ->
        let v = "v"^(string_of_int v) in
        let v = "(("^v^" Bool))" in
        String.concat " " ["(exists"; v; a; ")"]);
    mforall = (fun v a ->
        let v = "v"^(string_of_int v) in
        let v = "(("^v^" Bool))" in
        String.concat " " ["(forall"; v; a; ")"]);
    entry = (fun _ -> ());
    } f in
  Format.fprintf ff "%s" s


let pp_smtlib ff f =
  let sym ff i =
    Format.fprintf ff "v%d" i
  in
  ISet.iter (fun s ->
      Format.fprintf ff "(declare-fun %a () Bool)\n" sym s
    ) (symbols f);
  Format.fprintf ff "(assert ";
  pp_smtlib ff f;
  Format.fprintf ff ")"

let prenexcnf h e =
  let clauses = ref [] in
  let emit cl =
    clauses := cl :: !clauses
  in
  let frontier = ref 3 in
  let vtrue = 1 in
  let vfalse = 2 in
  let vnot v = -v in
  emit [vtrue];
  emit [vnot vfalse];
  let vmap = Hashtbl.create 8181 in

  let get_var v =
    try
      Hashtbl.find vmap v
    with Not_found ->
      let vid = !frontier in
      incr frontier;
      Hashtbl.replace vmap v vid;
      vid
  in

  let fresh () =
    let vid = !frontier in
    incr frontier;
    vid
  in

  let v = {
    mtrue = (vtrue, qtleaf);
    mfalse = (vfalse, qtleaf);
    mvar = (fun v -> (get_var v, qtleaf));
    mand = (fun (a,qa) (b,qb) ->
        let v = fresh () in
        (* v = a /\ b
           a /\ b -> v ---  ~a ~b v
           v -> a      ---  ~v a
           v -> b      ---  ~v b
        *)
        emit [-a; -b; v];
        emit [-v; a];
        emit [-v; b];
        (v, qtbranch qa qb));
    mor = (fun (a,qa) (b,qb) ->
        let v = fresh () in
        (* v = a \/ b
           v -> a \/ b --- ~v a b
           a -> v      --- ~a v
           b -> v      --- ~b v
        *)
        emit [a; b; -v];
        emit [v; -a];
        emit [v; -b];
        (v, qtbranch qa qb));
    mnot = (fun (a,qa) -> (-a, qtnot qa));

    mexists = (fun v (a,qa) ->
        let vid = Hashtbl.find vmap v in
        Hashtbl.remove vmap v;
        (a, qtexists vid qa)
      );
    mforall = (fun v (a,qa) ->
        let vid = Hashtbl.find vmap v in
        Hashtbl.remove vmap v;
        (a, qtforall vid qa)
      );
    entry = (fun e ->
        match e.H.node with
        | Exists (v, _)
        | ForAll (v, _) ->
          let vid = !frontier in
          incr frontier;
          Hashtbl.add vmap v vid
        | _ -> ()
      );
  } in
  let (id,qt) = visit (Hashtbl.create 8191) v e in
  emit [id];
  (vmap, !clauses, !frontier, qtlinearize qt)
  
