module L = LogicSymbolicSet

module Make
    (D: Interface.Domain
     with type sym = int
      and type cnstr = int L.t
      and type output = int L.t
      and type query = int L.q
    ) :
  Interface.Domain
  with type output = D.output
   and type cnstr = D.cnstr
   and type sym = D.sym
   and type query = D.query
= struct

  module U = UnionFind.MapBased.Make(struct
      type t = int
      let compare a b = b - a
    end)

  module SSet = U.ESet
  module SMap = U.EMap

  type ctx = {
    ce: U.ctx;
    cd: D.ctx;
  }

  type sym = D.sym
  type output = D.output
  type cnstr = D.cnstr
  type query = D.query

  type t = {
    d: D.t; (* non-equalities *)
    e: U.t; (* equalities *)
    dirty: bool; (* false if every symbol in d is a representative in e *)
  }

  let init () = {
    cd = D.init ();
    ce = U.init ();
  }

  let top ctx = {
    d = D.top ctx.cd;
    e = U.empty ctx.ce;
    dirty = false;
  }

  let bottom ctx = {
    d = D.bottom ctx.cd;
    e = U.empty ctx.ce;
    dirty = false;
  }

  let context t = {
    cd = D.context t.d;
    ce = U.context t.e;
  }

  let symbols {d;e} =
    let els = U.fold (fun el _ els -> SSet.add el els) e SSet.empty in
    let els = List.fold_left (fun els el -> SSet.add el els) els (D.symbols d) in
    U.ESet.elements els

  let serialize_eq t r =
    U.fold (fun el r d ->
        if el <> r then
          if d <> L.True then
            L.And(L.Eq(L.Var el, L.Var r),d)
          else
            L.Eq(L.Var el, L.Var r)
        else
          d
      ) t.e r

  let serialize t =
    let d = D.serialize t.d in
    serialize_eq t d

  let rec partition (eq,neq) = function
    | L.And(a,b) -> partition (partition (eq,neq) a) b
    | L.Eq(L.Var a, L.Var b)
    | L.Eq(L.Var a, L.Sing b)
    | L.Eq(L.Sing a, L.Var b)
    | L.Eq(L.Sing a, L.Sing b) -> ((a,b)::eq, neq)
    | L.True -> (eq,neq)
    | L.False -> ([], L.False)
    | c -> (eq, L.And(neq, c))

  let partition = partition ([], L.True)

  let constrain cnstr {d;e;dirty=_} =
    (* partition the constraints *)
    let (eq,neq) = partition cnstr in
    (* add equalities to the equality domain *)
    let e = List.fold_left (fun e (a,b) -> U.union a b e) e eq in
    let d = D.constrain neq d in
    {d;e;dirty=true}

  let remap_cnstr t cnstr =
    L.map_symbol (fun s -> U.rep s t.e) cnstr

  let remap t =
    let syms = lazy (List.fold_left (fun s e -> SSet.add e s) SSet.empty (D.symbols t.d)) in
    let rename = Rename.of_iter_mem_get
        (fun f -> List.iter (fun s -> f s (U.rep s t.e)) (D.symbols t.d))
        (fun s -> SSet.mem s (Lazy.force syms))
        (fun s -> U.rep s t.e) in
    if t.dirty then
      {
        d = D.rename_symbols rename t.d;
        e = t.e;
        dirty = false
      }
    else
      t

  let sat t cnstr =
    let t = remap t in
    (* partition the constraints *)
    let (eq,neq) = partition cnstr in
    let neq = remap_cnstr t neq in
    (* check equalities first *)
    let cnstr = List.fold_left (fun cnstr (a,b) ->
        if U.rep a t.e = U.rep b t.e then
          cnstr (* constraint already satisfied *)
        else
          L.And(L.Eq(L.Var a, L.Var b), cnstr)
      ) neq eq in
    (* remaining equalities are sent to the underlying domain *)
    D.sat t.d cnstr

  let is_bottom t =
    let t = remap t in
    D.is_bottom t.d

  let le a b =
    let a = remap a in
    let b = remap b in
    (* go through each equality in b.  If it is not in a.eq, add to a constraint *)
    let cnstr = U.fold (fun e1 _ cnstr ->
        let e2 = U.rep e1 b.e in
        let r1 = U.rep e1 a.e in
        let r2 = U.rep e2 a.e in
        if r1 <> r2 then
          L.And(cnstr, L.Eq(L.Var e1, L.Var e2))
        else
          cnstr
      ) b.e L.True in
    let bd = D.constrain cnstr b.d in
    D.le a.d bd

  let upper_bound op a b =
    let e = U.split a.e b.e in
    let da = U.diff e a.e in
    let db = U.diff e b.e in
    let strengthen c (a,b) = L.And(c,L.Eq(L.Var a, L.Var b)) in
    let ca = List.fold_left strengthen L.True da in
    let cb = List.fold_left strengthen L.True db in
    let doma = D.constrain ca a.d in
    let domb = D.constrain cb b.d in
    let d = op doma domb in
    {d;e;dirty=true}



  let join a b =
    upper_bound D.join a b

  let widening a b =
    upper_bound D.widening a b

  let meet a b =
    let e = U.merge a.e b.e in
    let d = D.meet a.d b.d in
    {d;e;dirty=true}

  let forget syms t =
    let t = remap t in
    let e,renames,syms = List.fold_left (fun (e,c,syms) s ->
        match U.remove s e with
        | e, U.NoRepresentative ->
          (* it was a representative, but there are no equalities *)
          (e,c,s::syms)
        | e, U.SameRepresentative ->
          (* it was not a representative, it is not in the underlying domain *)
          (e,c,syms)
        | e, U.NewRepresentative r ->
          (* it is a representative and there is an equality, do a replacement
             in the underlying domain *)
          (e, Rename.append c (Rename.singleton s r), syms)
      ) (t.e,Rename.empty,[]) syms in
    let d = t.d |>
            D.forget syms |>
            D.rename_symbols (Rename.of_composition renames) in
    {e;d;dirty=false}

  let rename_symbols map t =
    {
      e = U.rename map t.e;
      d = D.rename_symbols map t.d;
      dirty = true;
    }

  let query t =
    {
      L.get_eqs = (fun () -> U.pairs t.e);
      L.get_eqs_sym = (fun s ->
          let r = U.rep s t.e in
          U.elements r t.e |>
          SSet.remove s |>
          SSet.add r |>
          SSet.elements
        );
    }

  let combine q t =
    {t with
     e = List.fold_left (fun e (a,b) -> U.union a b e) t.e (q.L.get_eqs ());
     dirty = true;
    }

  let pp_debug pp_sym ff t =
    let ps = U.pairs t.e in
    Format.fprintf ff "@[<v 0>@[<hv 2>eqs:@ %a@]@,dom: %a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ff (a,b) -> Format.fprintf ff "%a = %a" pp_sym a pp_sym b)) ps
      (D.pp_print pp_sym) t.d


  let pp_print pp_sym ff t =
    begin match serialize_eq t L.True with
    | L.True -> ()
    | eqs ->
      Format.fprintf ff "%a ∧ " (L.pp pp_sym) eqs
    end;
    D.pp_print pp_sym ff t.d
end

