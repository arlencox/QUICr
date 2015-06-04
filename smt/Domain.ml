module L = LogicSymbolicSet

type sym = int
type cnstr = int L.t
type output = int L.t
type query = int L.q

type ctx = {
  c: Z3.context;
  s: Z3.Solver.solver;
  counter: int ref;
}

let init () =
  let c = Z3.mk_context [] in
  let s = Z3.Solver.mk_simple_solver c in
  {
    counter = ref 0;
    c;
    s;
  }

let fresh_count ctx =
  let res = !(ctx.counter) in
  incr ctx.counter;
  res

module Int = struct
  type t = int
  let compare = compare
end

module ISet = Set.Make(Int)
module IMap = Map.Make(Int)

type t = {
  ctx: ctx;
  syms: int IMap.t;
  expr: Z3.Expr.expr;
}

let top ctx =
  {
    ctx = ctx;
    syms = IMap.empty;
    expr = Z3.Boolean.mk_true ctx.c;
  }

let bottom ctx =
  {
    ctx = ctx;
    syms = IMap.empty;
    expr = Z3.Boolean.mk_false ctx.c;
  }


let context t =
  t.ctx

let symbols t =
  List.map fst (IMap.bindings t.syms)

let get_variable ctx id =
  (*let str = "sym_"^(string_of_int id) in
  let sym = Z3.Symbol.mk_string ctx.c str in*)
  let sym = Z3.Symbol.mk_int ctx.c id in
  let b = Z3.Boolean.mk_sort ctx.c in
  let d = Z3.FuncDecl.mk_const_decl ctx.c sym b in
  Z3.FuncDecl.apply d []

exception Unsupported

let rec of_expr map ctx = function
  | L.Empty ->
    (Z3.Boolean.mk_false ctx.c, Z3.Boolean.mk_true ctx.c)
  | L.Universe ->
    (Z3.Boolean.mk_true ctx.c, Z3.Boolean.mk_true ctx.c)
  | L.DisjUnion (a,b) -> 
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    (* A ^ B = 0
       A /\ B = false
       ~(A /\ B) *)
    (Z3.Boolean.mk_or ctx.c [a; b],
     (Z3.Boolean.mk_and ctx.c [
         ac; bc;
         Z3.Boolean.mk_not ctx.c (Z3.Boolean.mk_and ctx.c [a; b])
       ]))
  | L.Union (a,b) ->
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    (Z3.Boolean.mk_or ctx.c [a; b], Z3.Boolean.mk_and ctx.c [ac; bc])
  | L.Inter (a,b) ->
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    (Z3.Boolean.mk_and ctx.c [a; b], Z3.Boolean.mk_and ctx.c [ac; bc])
  | L.Diff (a,b) ->
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    (Z3.Boolean.mk_and ctx.c [a; Z3.Boolean.mk_not ctx.c b], Z3.Boolean.mk_and ctx.c [ac; bc])
  | L.Comp a ->
    let (a,ac) = of_expr map ctx a in
    (Z3.Boolean.mk_not ctx.c a, ac)
  | L.Var v
  | L.Sing v ->
    let id = try
        IMap.find v !map
      with Not_found ->
        let id = fresh_count ctx in
        map := IMap.add v id !map;
        id
    in
    let e = get_variable ctx id in
    (e, Z3.Boolean.mk_true ctx.c)

let rec of_cnstr map is_pos is_over ctx c =
  let r is_inv = of_cnstr map is_inv is_over ctx in
  match c, is_pos with
  | L.Eq (a,b), true ->
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    Z3.Boolean.mk_and ctx.c [
      Z3.Boolean.mk_eq ctx.c a b;
      ac;
      bc
    ]
  | L.Eq _, false ->
    if is_over then
      Z3.Boolean.mk_true ctx.c
    else
      raise Unsupported
  | L.SubEq (a,b), true ->
    let (a,ac) = of_expr map ctx a in
    let (b,bc) = of_expr map ctx b in
    Z3.Boolean.mk_and ctx.c [
      Z3.Boolean.mk_implies ctx.c a b;
      ac;
      bc
    ]
  | L.SubEq _, false ->
    if is_over then
      Z3.Boolean.mk_true ctx.c
    else
      raise Unsupported
  | L.In (av,b), _ ->
    if is_over then
      Z3.Boolean.mk_true ctx.c
    else
      raise Unsupported
  | L.And (a,b), true ->
    let a = r is_pos a in
    let b = r is_pos b in
    Z3.Boolean.mk_and ctx.c [a; b]

  | L.And (a,b), false ->
    let a = r is_pos a in
    let b = r is_pos b in
    if is_over then
      Z3.Boolean.mk_or ctx.c [a; b]
    else 
      raise Unsupported
  | L.Not a, _ ->
    r (not is_pos) a
  | L.True, false
  | L.False, true ->
    Z3.Boolean.mk_false ctx.c
  | L.False, false
  | L.True, true ->
    Z3.Boolean.mk_true ctx.c

let constrain cnstr t =
  let ctx = t.ctx in
  let map = ref t.syms in
  let c = of_cnstr map true true ctx cnstr in
  {
    expr = Z3.Boolean.mk_and ctx.c [c; t.expr];
    syms = !map;
    ctx;
  }

let is_valid ctx e =
  let e = Z3.Boolean.mk_not ctx.c e in
  (*Format.printf "Valid? %s@." (Z3.Expr.to_string (Z3.Expr.simplify e None));*)
  (*let s = Z3.Solver.mk_simple_solver ctx.c in*)
  let id = fresh_count ctx in
  let v = get_variable ctx id in
  Z3.Solver.add ctx.s [Z3.Boolean.mk_eq ctx.c v e];
  match Z3.Solver.check ctx.s [v] with
  | Z3.Solver.UNSATISFIABLE -> true
  | Z3.Solver.SATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> failwith "Solver returned unknown"


let sat t cnstr =
  try
    let ctx = t.ctx in
    let map = ref t.syms in
    let c = of_cnstr map true false (context t) cnstr in
    (*Format.printf "@. SAT: %s@." (Z3.Expr.to_string c);*)
    is_valid ctx (Z3.Boolean.mk_implies ctx.c t.expr c)
  with Unsupported ->
    false

let is_top t =
  is_valid t.ctx t.expr

let is_bottom t =
  is_valid t.ctx (Z3.Boolean.mk_not t.ctx.c t.expr)

let add_eq ctx eqs expr =
  List.fold_left (fun e (a,b) ->
      let a = get_variable ctx a in
      let b = get_variable ctx b in
      Z3.Boolean.mk_and ctx.c [e; Z3.Boolean.mk_eq ctx.c a b]
    ) expr eqs

let project_out ctx ids expr =
  if ISet.is_empty ids then
    expr
  else
    (*let names = List.rev_map (Z3.Symbol.mk_int ctx.c) (ISet.elements ids) in
    let b = Z3.Boolean.mk_sort ctx.c in
    let sorts = List.rev_map (fun _ -> b) names in
    let q = Z3.Quantifier.mk_exists ctx.c sorts names expr None [] [] None None in
    Z3.Quantifier.expr_of_quantifier q*)
    let ids = List.rev_map (get_variable ctx) (ISet.elements ids) in
    (*List.iter (fun e ->
        Format.printf "Projecting %s@." (Z3.Expr.to_string e)
      ) ids;*)
    let q = Z3.Quantifier.mk_exists_const ctx.c ids expr None [] [] None None in
    Z3.Quantifier.expr_of_quantifier q


let le t1 t2 =
  let ctx = t1.ctx in
  let used = ref ISet.empty in
  let projs = ref ISet.empty in
  let eqs = ref [] in

  ignore (IMap.merge (fun v i1 i2 ->
      begin match i1, i2 with
      | Some i1, Some i2 when i1 = i2 ->
        used := ISet.add i1 !used
      | Some i1, Some i2 ->
        eqs := (i1,i2) :: !eqs;
        projs := ISet.add i1 !projs;
        used := ISet.add i2 !used
      | Some i1, None ->
        projs := ISet.add i1 !projs
      | None, Some i2 ->
        used := ISet.add i2 !used
      | None, None -> ()
      end;
      None
    ) t1.syms t2.syms);
  let used = !used in
  let projs = !projs in
  let eqs = !eqs in
  let projs = ISet.diff projs used in
  let t1e = t1.expr |>
            add_eq ctx eqs |>
            project_out ctx projs in
  let v = Z3.Boolean.mk_implies ctx.c t1e t2.expr in
  is_valid ctx v

let unify_environment t1 t2 =
  let ctx = t1.ctx in
  let t1used = ref ISet.empty in
  let t2used = ref ISet.empty in
  let t1proj = ref ISet.empty in
  let t2proj = ref ISet.empty in
  let t1eqs = ref [] in
  let t2eqs = ref [] in
  let syms = IMap.merge (fun v i1 i2 ->
      match i1, i2 with
      | Some i1, Some i2 when i1 = i2 ->
        t1used := ISet.add i1 !t1used;
        t2used := ISet.add i1 !t2used;
        Some i1
      | Some i1, Some i2 ->
        let id = fresh_count ctx in
        t1used := ISet.add id !t1used;
        t2used := ISet.add id !t2used;
        t1proj := ISet.add i1 !t1proj;
        t2proj := ISet.add i2 !t2proj;
        t1eqs  := (i1,id) :: !t1eqs;
        t2eqs  := (i2,id) :: !t2eqs;
        Some id
      | Some i1, None ->
        let id = fresh_count ctx in
        t1used := ISet.add id !t1used;
        t1proj := ISet.add i1 !t1proj;
        t1eqs  := (i1,id) :: !t1eqs;
        Some id
      | None, Some i2 ->
        let id = fresh_count ctx in
        t2used := ISet.add id !t2used;
        t2proj := ISet.add i2 !t2proj;
        t2eqs  := (i2,id) :: !t2eqs;
        Some id
      | None, None -> None
    ) t1.syms t2.syms in
  let t1projs = ISet.diff !t1proj !t1used in
  let t2projs = ISet.diff !t2proj !t2used in
  let t1eqs = !t1eqs in
  let t2eqs = !t2eqs in
  let t1e = t1.expr |>
            add_eq ctx t1eqs |>
            project_out ctx t1projs in
  let t2e = t2.expr |>
            add_eq ctx t2eqs |>
            project_out ctx t2projs in
  (syms, t1e, t2e)

let join t1 t2 =
  let (syms, t1e, t2e) = unify_environment t1 t2 in
  let expr = Z3.Boolean.mk_or t1.ctx.c [t1e; t2e] in
  let expr = Z3.Expr.simplify expr None in
  {
    ctx=t1.ctx;
    expr;
    syms;
  }

let widening = join

let meet t1 t2 = 
  let (syms, t1e, t2e) = unify_environment t1 t2 in
  let expr = Z3.Boolean.mk_and t1.ctx.c [t1e; t2e] in
  let expr = Z3.Expr.simplify expr None in
  {
    ctx=t1.ctx;
    expr;
    syms;
  }

let forget syms t =
  let proj,syms = List.fold_left (fun (proj,syms) s ->
      try
        let id = IMap.find s t.syms in
        (ISet.add id proj, IMap.remove s syms)
      with Not_found ->
        (proj, syms)
    ) (ISet.empty,t.syms) syms in
  {t with
   expr = project_out t.ctx proj t.expr;
   syms
  }

let rename_symbols rename t =
  let old_ids = IMap.fold (fun _ id ids -> ISet.add id ids) t.syms ISet.empty in
  let syms = Rename.fold (fun a b syms ->
      let id = try IMap.find a t.syms with Not_found -> fresh_count t.ctx in
      IMap.add b id syms
    ) rename t.syms in
  let new_ids = IMap.fold (fun _ id ids -> ISet.add id ids) syms ISet.empty in
  let expired_ids = ISet.diff old_ids new_ids in
  let expr = project_out t.ctx expired_ids t.expr in
  {t with expr; syms}

let query t =
  {
    L.get_eqs = (fun () -> []);
    L.get_eqs_sym = (fun s -> []);
  }

let combine q t =
  List.fold_left (fun t (s1, s2) ->
      constrain (L.Eq (L.Var s1, L.Var s2)) t
    ) t (q.L.get_eqs ())

let and_list l =
  List.fold_right (fun e r ->
      match e,r with
      | L.True,o
      | o, L.True -> o
      | _ -> L.And (e, r)
    ) l L.True

let union_list l =
  List.fold_right (fun e r ->
      match r with
      | L.Empty -> e
      | _ -> L.Union (e, r)
    ) l L.Empty

let inter_list l =
  List.fold_right (fun e r ->
      match r with
      | L.Universe -> e
      | _ -> L.Inter (e, r)
    ) l L.Universe

let rec fold_pairs f l r =
  match l with
  | h::t ->
    let r = List.fold_left (fun r e2 -> f h e2 r) r t in
    fold_pairs f t r
  | [] -> r

  

let rec to_expr rmap e =
  let decl = Z3.Expr.get_func_decl e in
  let args = Z3.Expr.get_args e in
  match Z3.FuncDecl.get_decl_kind decl, args with
  | Z3enums.OP_AND, args ->
    let args = List.map (to_expr rmap) args in
    inter_list args
  | Z3enums.OP_OR, args ->
    let args = List.map (to_expr rmap) args in
    union_list args
  | Z3enums.OP_NOT, [a] ->
    let a = to_expr rmap a in
    L.Comp a
  | Z3enums.OP_TRUE, [] ->
    L.Universe
  | Z3enums.OP_FALSE, [] ->
    L.Empty
  | Z3enums.OP_UNINTERPRETED, [] ->
    let sym = Z3.FuncDecl.get_name decl in
    assert(Z3.Symbol.is_int_symbol sym);
    let id = Z3.Symbol.get_int sym in
    let v = try IMap.find id rmap with Not_found -> -id in
    L.Var v
  | _ ->
    failwith ("unsupported to_expr: "^(Z3.Expr.to_string e))

let rec to_constrain rmap e  =
  (* detect quantifier *)
  if Z3.AST.is_quantifier (Z3.Expr.ast_of_expr e) then begin
    let q = Z3.Quantifier.quantifier_of_expr e in
    (*let is_ex = Z3.Quantifier.is_existential q in*)
    (*let bvs = Z3.Quantifier.get_bound_variable_names q in*)
    let e = Z3.Quantifier.get_body q in
    let e = to_constrain rmap e in
    e
  end else begin
    let decl = Z3.Expr.get_func_decl e in
    let args = Z3.Expr.get_args e in
    match Z3.FuncDecl.get_decl_kind decl, args with
    | Z3enums.OP_AND, args ->
      let args = List.map (to_constrain rmap) args in
      and_list args
    | Z3enums.OP_OR, args ->
      let args = List.map (to_constrain rmap) args in
      let args = List.map (fun e -> L.Not e) args in
      L.Not (and_list args)
    | Z3enums.OP_NOT, [a] ->
      let a = to_constrain rmap a in
      L.Not a
    | Z3enums.OP_EQ, [a; b] ->
      let a = to_expr rmap a in
      let b = to_expr rmap b in
      L.Eq(a,b)
    | Z3enums.OP_DISTINCT, args ->
      let args = List.map (to_expr rmap) args in
      let args = fold_pairs (fun e1 e2 l -> (L.Eq(L.Inter(e1,e2),L.Empty))::l) args [] in
      and_list args
    | Z3enums.OP_IMPLIES, [a; b] ->
      let a = to_expr rmap a in
      let b = to_expr rmap b in
      L.SubEq(a,b)
    | Z3enums.OP_TRUE, [] ->
      L.True
    | Z3enums.OP_FALSE, [] ->
      L.False
    | _ ->
      failwith ("unsupported to_constrain: "^(Z3.Expr.to_string e))
  end


let serialize t =
  let (rmap,eqs) = IMap.fold (fun v id (rmap,eqs) ->
      try
        let vo = IMap.find id rmap in
        (rmap, L.Eq(L.Var v, L.Var vo)::eqs)
      with Not_found ->
        (IMap.add id v rmap, eqs)
    ) t.syms (IMap.empty,[]) in
  let expr = t.expr in
  (*let expr = Z3.Expr.simplify expr None in*)
  let c = to_constrain rmap expr in
  and_list (c::eqs)

let pp_print pp_sym ff t =
  (*let (rmap,eqs) = IMap.fold (fun v id (rmap,eqs) ->
      try
        let vo = IMap.find id rmap in
        (rmap, L.Eq(L.Var v, L.Var vo)::eqs)
      with Not_found ->
        (IMap.add id v rmap, eqs)
    ) t.syms (IMap.empty,[]) in*)
  let expr = t.expr in
  let expr = Z3.Expr.simplify expr None in
  let s = Z3.Expr.to_string expr in

  IMap.iter (fun v id ->
      Format.fprintf ff "%a := %s@," pp_sym v (Z3.Expr.to_string (get_variable t.ctx id))
    ) t.syms;
  Format.fprintf ff "%s" s
  (*let s = serialize t in*)
  (*LogicSymbolicSet.pp pp_sym ff s*)

let pp_debug = pp_print
