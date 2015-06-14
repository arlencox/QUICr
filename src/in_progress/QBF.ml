
module L = LogicSymbolicSet
module F = QBF_Internal.Formula

module type Solver = sig
  val is_valid : F.h -> Formula.c -> bool
end


module Make(S: Solver) : Interface.Domain
  with type sym = int
   and type cnstr = int L.t
   and type output = int L.t
   and type query = int L.q
= struct

  type sym = int

  type cnstr = int L.t
  type output = int L.t
  type query = int L.q

  type ctx = F.h

  let init () = F.create 8191

  type t = {
    ctx: ctx;
    e: F.c;
  }

  let top ctx =
    { ctx; e = F.mtrue ctx}

  let bottom ctx =
    { ctx; e = F.mfalse ctx}

  let context t = t.ctx

  let symbols t =
    F.ISet.elements @@ F.symbols t.e

  let rec of_expr ctx = function
    | L.Empty ->
      (F.mfalse ctx, F.mtrue ctx)
    | L.Universe ->
      (F.mtrue ctx, F.mtrue ctx)
    | L.DisjUnion (a,b) -> 
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (* A ^ B = 0
         A /\ B = false
         ~(A /\ B) *)
      (F.mor ctx a b, (F.mand ctx (F.mand ctx ac bc) (F.mnot ctx (F.mand ctx a b))))
    | L.Union (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (F.mor ctx a b, F.mand ctx ac bc)
    | L.Inter (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (F.mand ctx a b, F.mand ctx ac bc)
    | L.Diff (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (F.mand ctx a (F.mnot ctx b), F.mand ctx ac bc)
    | L.Comp a ->
      let (a,ac) = of_expr ctx a in
      (F.mnot ctx a, ac)
    | L.Var v ->
      (F.mvar ctx v, F.mtrue ctx)
    | L.Sing v ->
      (F.mvar ctx v, F.mtrue ctx)

  exception Unsupported

  let rec of_cnstr is_pos is_over ctx c =
    let r is_inv = of_cnstr is_inv is_over ctx in
    match c, is_pos with
    | L.Eq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      F.mand ctx (F.meq ctx a b) (F.mand ctx ac bc)
    | L.Eq _, false ->
      if is_over then
        F.mtrue ctx
      else
        raise Unsupported
    | L.SubEq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      F.mand ctx (F.mimply ctx a b) (F.mand ctx ac bc)
    | L.SubEq _, false ->
      if is_over then
        F.mtrue ctx
      else
        raise Unsupported
    | L.In (av,b), _ ->
      if is_over then
        F.mtrue ctx
      else
        raise Unsupported
    | L.And (a,b), true ->
      let a = r is_pos a in
      let b = r is_pos b in
      F.mand ctx a b

    | L.And (a,b), false ->
      let a = r is_pos a in
      let b = r is_pos b in
      if is_over then
        F.mor ctx a b
      else 
        raise Unsupported
    | L.Not a, _ ->
      r (not is_pos) a
    | L.True, false
    | L.False, true ->
      F.mfalse ctx
    | L.False, false
    | L.True, true ->
      F.mtrue ctx

  let constrain cnstr t =
    let c = of_cnstr true true (context t) cnstr in
    {t with e = F.mand t.ctx c t.e}

  let serialize t =
    failwith "SAT.Domain.serialize unimplemented"

  let join a b = {a with e = F.mor a.ctx a.e b.e}

  let widening = join

  let meet a b = {a with e = F.mand a.ctx a.e b.e}

  let sat t cnstr =
    try
      let c = of_cnstr true false (context t) cnstr in
      S.is_valid t.ctx (F.mimply t.ctx t.e c)
      (*MLBDD.is_true (MLBDD.imply t c)*)
    with Unsupported ->
      false

  let le a b =
    S.is_valid a.ctx (F.mimply a.ctx a.e b.e)

  let forget syms t =
    {t with e = List.fold_left (fun e v -> F.mexists t.ctx v e) t.e syms }

  let is_top t =
    S.is_valid t.ctx t.e

  let is_bottom t =
    S.is_valid t.ctx (F.mnot t.ctx t.e)

  let rec rename_symbols ctx visited rename ignore e =
    let rign = rename_symbols ctx visited rename in
    let rrec = rign ignore in
    try
      Hashtbl.find visited e.F.H.tag
    with Not_found ->
      let result = match e.F.H.node with
        | F.True -> F.mtrue ctx
        | F.False -> F.mfalse ctx
        | F.Var x ->
          if F.ISet.mem x ignore then
            F.mvar ctx x
          else
            F.mvar ctx (Rename.get rename x)
        | F.And (a,b) ->
          let a = rrec a in
          let b = rrec b in
          F.mand ctx a b
        | F.Or (a,b) ->
          let a = rrec a in
          let b = rrec b in
          F.mor ctx a b
        | F.Not a ->
          let a = rrec a in
          F.mnot ctx a
        | F.Exists (x,a) ->
          let a = rign (F.ISet.add x ignore) a in
          F.mexists ctx x a
        | F.ForAll (x,a) ->
          let a = rign (F.ISet.add x ignore) a in
          F.mforall ctx x a
      in
      Hashtbl.replace visited e.F.H.tag result;
      result

  let rename_symbols rename t =
    {t with e = rename_symbols t.ctx (Hashtbl.create 8191) rename F.ISet.empty t.e }

  (*let rename_symbols rename t =
    let h = t.ctx in
    {t with e = Rename.fold (fun a b e ->
         F.mexists h a (F.mand h (F.meq h (F.mvar h a) (F.mvar h b)) e)
       ) rename t.e}*)

  let query t =
    {
      L.get_eqs = (fun () -> []);
      L.get_eqs_sym = (fun s -> []);
    }

  let combine q t =
    List.fold_left (fun t (s1, s2) ->
        constrain (L.Eq (L.Var s1, L.Var s2)) t
      ) t (q.L.get_eqs ())

  let pp_print pp_sym ff t =
    F.pp_formula pp_sym ff t.e

  let pp_debug = pp_print

end
