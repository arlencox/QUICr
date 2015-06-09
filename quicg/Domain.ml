module L = LogicSymbolicSet
module D = BottomDomain.Make(SetDomain.Make(TopDomain))

module Int = struct
  type t = int
  let compare = compare
end

module ISet = Set.Make(Int)

type ctx = D.ctx

type t = {
  ctx: ctx;
  d: D.t;
  vars: ISet.t;
}

type sym = int
type cnstr = int L.t
type output = int L.t
type query = int L.q

let init = D.init

let top ctx = {
  ctx;
  d = D.top ctx 0 0;
  vars = ISet.empty;
}

let bottom ctx = {
  ctx;
  d = D.bottom ctx 0 0;
  vars = ISet.empty;
}

let context t = 
  t.ctx

let symbols t =
  ISet.elements t.vars

let rec of_expr = function
  | L.Empty ->
    (DS.CNF.mk_false, DS.CNF.mk_true)
  | L.Universe ->
    (DS.CNF.mk_true, DS.CNF.mk_true)
  | L.DisjUnion (a,b) -> 
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    (* A ^ B = 0
       A /\ B = false
       ~(A /\ B) *)
    (DS.CNF.mk_or a b, (DS.CNF.mk_and (DS.CNF.mk_and ac bc) (DS.CNF.mk_not (DS.CNF.mk_and a b))))
  | L.Union (a,b) ->
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    (DS.CNF.mk_or a b, DS.CNF.mk_and ac bc)
  | L.Inter (a,b) ->
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    (DS.CNF.mk_and a b, DS.CNF.mk_and ac bc)
  | L.Diff (a,b) ->
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    (DS.CNF.mk_and a (DS.CNF.mk_not b), DS.CNF.mk_and ac bc)
  | L.Comp a ->
    let (a,ac) = of_expr a in
    (DS.CNF.mk_not a, ac)
  | L.Var v ->
    (DS.CNF.mk_var v, DS.CNF.mk_true)
  | L.Sing v ->
    (DS.CNF.mk_var v, DS.CNF.mk_true)

exception Unsupported

let rec of_cnstr is_pos is_over c =
  let r is_inv = of_cnstr is_inv is_over in
  match c, is_pos with
  | L.Eq (a,b), true ->
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    DS.CNF.mk_and (DS.CNF.mk_eq a b) (DS.CNF.mk_and ac bc)
  | L.Eq _, false ->
    if is_over then
      DS.CNF.mk_true
    else
      raise Unsupported
  | L.SubEq (a,b), true ->
    let (a,ac) = of_expr a in
    let (b,bc) = of_expr b in
    DS.CNF.mk_and (DS.CNF.mk_imply a b) (DS.CNF.mk_and ac bc)
  | L.SubEq _, false ->
    if is_over then
      DS.CNF.mk_true
    else
      raise Unsupported
  | L.In (av,b), _ ->
    if is_over then
      DS.CNF.mk_true
    else
      raise Unsupported
  | L.And (a,b), true ->
    let a = r is_pos a in
    let b = r is_pos b in
    DS.CNF.mk_and a b

  | L.And (a,b), false ->
    let a = r is_pos a in
    let b = r is_pos b in
    if is_over then
      DS.CNF.mk_or a b
    else 
      raise Unsupported
  | L.Not a, _ ->
    r (not is_pos) a
  | L.True, false
  | L.False, true ->
    DS.CNF.mk_false
  | L.False, false
  | L.True, true ->
    DS.CNF.mk_true

let constrain cnstr t =
  let c = of_cnstr true true cnstr in
  let syms = DS.CNF.symbols c in
  let vars = List.fold_left (fun vars sym ->
      ISet.add sym vars
    ) t.vars syms in
  let ctx = t.ctx in
  let d = List.fold_left (fun d clause ->
      let (a,b) = List.fold_left (fun (n,p) (b,v) ->
          if b then
            (n,v::p)
          else
            (v::n,p)
        ) ([],[]) clause in
      D.constrain_qc ctx d a b
    ) t.d c in
  {
    ctx;
    vars;
    d;
  }

let serialize t =
  failwith "Serialize unimplemented"

let sat t cnstr =
  try
    let c = of_cnstr true false cnstr in
    let ctx = t.ctx in
    let d = D.top ctx 0 0 in
    let d = List.fold_left (fun d clause ->
        let (a,b) = List.fold_left (fun (n,p) (b,v) ->
            if b then
              (n,v::p)
            else
              (v::n,p)
          ) ([],[]) clause in
        D.constrain_qc ctx d a b
      ) d c in
    D.le ctx t.d d
  with Unsupported ->
    false

let bound op a b =
  {
    ctx=a.ctx;
    d = op a.ctx a.d b.d;
    vars = ISet.union a.vars b.vars;
  }

let join a b =
  bound D.join a b

let widening a b =
  bound D.widen a b

let meet a b =
  bound D.meet a b

let le a b =
  D.le a.ctx a.d b.d

let is_bottom t =
  D.is_bottom t.ctx t.d

let is_top t =
  D.is_top t.ctx t.d

let forget syms t =
  let ctx = t.ctx in
  let d = List.fold_left (fun d sym -> D.forget_set ctx d sym) t.d syms in
  let vars = List.fold_left (fun vars sym -> ISet.remove sym vars) t.vars syms in
  { d; ctx; vars }

let rename_symbol a b t =
  let vars = t.vars |>
             ISet.remove a |>
             ISet.add b in
  {t with
   vars;
   d = D.rename_set t.ctx t.d a b}

let rename_symbols rename t =
  let counter = ref (ISet.max_elt t.vars) in
  let fresh () =
    incr counter;
    !counter
  in

  let ftot = Hashtbl.create 1023 in
  let ttof = Hashtbl.create 1023 in

  (* parallel rename to serial rename:
     a -> b
     b -> a
     c -> b

     a -> e
     b -> d
     c -> e
     e -> b
     d -> a
  *)
  let t = Rename.fold (fun a b t ->
      let tgt = try
          Hashtbl.find ttof b
        with Not_found ->
          let f = fresh () in
          Hashtbl.replace ttof b f;
          Hashtbl.replace ftot f b;
          f
      in
      rename_symbol a tgt t
    ) rename t in
  let t = Hashtbl.fold (fun f tgt t ->
      rename_symbol f tgt t
    ) ftot t in
  t

let query t =
  {
    L.get_eqs = (fun () -> []);
    L.get_eqs_sym = (fun s -> []);
  }

let combine q t =
  List.fold_left (fun t (s1, s2) ->
      constrain (L.Eq (L.Var s1, L.Var s2)) t
    ) t (q.L.get_eqs ())

let pp_debug pp_sym ff t =
  D.fmt t.ctx pp_sym pp_sym ff t.d

let pp_print pp_sym ff t =
  D.fmt t.ctx pp_sym pp_sym ff t.d

