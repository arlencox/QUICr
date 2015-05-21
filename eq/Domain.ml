module L = LogicSymbolicSet

let debug = true
(*let debug = false*)

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
    s: SSet.t; (* symbols managed by in d *)
    e: U.t; (* equalities *)
  }

  let check msg t =
    let syms = List.fold_left (fun syms s -> SSet.add s syms) SSet.empty (D.symbols t.d) in
    let s_subset_of_syms = SSet.subset syms t.s in
    if not s_subset_of_syms then begin
      Format.printf "s not subet of syms: %s@." msg;
      Format.printf " s    = %a@." (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int) (SSet.elements t.s);
      Format.printf " syms = %a@." (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int) (D.symbols t.d);
    end;
    assert(s_subset_of_syms);
    assert(SSet.for_all (fun s ->
        let r = U.rep s t.e in
        let sym_is_rep = r = s in
        if not sym_is_rep then begin
          Format.printf "sym is not rep: %d;   rep = %d: %s@." s r msg
        end;
        sym_is_rep
      ) syms)

  let init () = {
    cd = D.init ();
    ce = U.init ();
  }

  let top ctx = {
    d = D.top ctx.cd;
    s = SSet.empty;
    e = U.empty ctx.ce;
  }

  let bottom ctx = {
    d = D.bottom ctx.cd;
    s = SSet.empty;
    e = U.empty ctx.ce;
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

  let remap_cnstr e cnstr =
    L.map_symbol (fun s -> U.rep s e) cnstr

  let symbols_cnstr cnstr =
    let syms = ref SSet.empty in
    L.iter_sym (fun _sing s -> syms := SSet.add s !syms) cnstr;
    !syms

  let fold_pair_els f r l =
    List.fold_left (fun r (a,b) -> f (f r a) b) r l

  let constrain cnstr {d;e;s} =
    (* partition the constraints *)
    let (eq,neq) = partition cnstr in
    (* save the original equalities *)
    let orig_e = e in
    (* compute new equalities *)
    let e = List.fold_left (fun e (a,b) ->
        U.union a b e
      ) e eq in
    (* compute representatives that may have changed *)
    let ren = fold_pair_els (fun ren a ->
        let r : sym = U.rep a orig_e in
        if SSet.mem r s then
          let r' = U.rep r e in
          if r != r' then
            (r,r') :: ren
          else
            ren
        else
          ren
      ) [] eq in
    (* do a rename on d if necessary *)
    let d = if ren = [] then d else
        D.rename_symbols (Rename.of_assoc_list ren) d in
    (* constrain d with the non-equality constraints after they have had their symbols remapped *)
    let neq = remap_cnstr e cnstr in
    let d = D.constrain neq d in
    (* recompute symbols of d *)
    let s = List.fold_left (fun s (r,_r') -> SSet.remove r s) s ren in
    let s = SSet.union s (symbols_cnstr neq) in

    let t = {d;e;s} in
    if debug then check "constrain" t;
    t
      


  let sat t cnstr =
    (* partition the constraints *)
    let (eq,neq) = partition cnstr in
    let neq = remap_cnstr t.e neq in
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
    D.is_bottom t.d

  let le a b =
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
    let s = a.s |>
            SSet.union b.s |>
            SSet.union (symbols_cnstr ca) |>
            SSet.union (symbols_cnstr cb)
    in
    let t = {d;e;s} in
    if debug then check "upper bound" t;
    t



  let join a b =
    upper_bound D.join a b

  let widening a b =
    upper_bound D.widening a b

  let remap e d =
    let s = List.fold_left (fun s e -> SSet.add e s) SSet.empty (D.symbols d) in
    let rename = Rename.of_iter_mem_get
        (fun f -> SSet.iter (fun sym -> f sym (U.rep sym e)) s)
        (fun sym -> SSet.mem sym s)
        (fun sym -> U.rep sym e) in
    let d = D.rename_symbols rename d in
    let s = List.fold_left (fun s e -> SSet.add e s) SSet.empty (D.symbols d) in
    let t = {d;e;s} in
    if debug then check "remap" t;
    t

  let meet a b =
    (*let cb = serialize b in*)
    (*constrain cb a*)
    let e = U.merge a.e b.e in
    let d = D.meet a.d b.d in
    (* TODO: a better implementation that doesn't do a full remap *)
    remap e d

  let forget syms t =
    let e,renames,syms = List.fold_left (fun (e,c,syms) s ->
        match U.remove s e with
        | e, U.NoRepresentative ->
          (* it was a representative, but there are no equalities *)
          (e,c,s::syms)
        | e, U.SameRepresentative ->
          (* it was not a representative, it is not in the underlying domain *)
          (e,c,syms)
        | e, U.NewRepresentative r when SSet.mem s t.s ->
          (* it is a representative and there is an equality, do a replacement
             in the underlying domain *)
          (e, Rename.append c (Rename.singleton s r), syms)
        | e, U.NewRepresentative r ->
          (* if the new representative is not in the domain, skip any renaming
          *)
          (e,c,syms)
      ) (t.e,Rename.empty,[]) syms in
    let d = t.d in
    let d = if syms <> [] then D.forget syms d else d in
    let d = if Rename.is_empty renames then d else
        D.rename_symbols (Rename.of_composition renames) d in
    (* FIXME: do a better computation of symbols *)
    let s = List.fold_left (fun s e -> SSet.add e s) SSet.empty (D.symbols d) in
    let t = {e;d;s} in
    if debug then check "forget" t;
    t

  let rename_symbols map t =
    let e = U.rename map t.e in
    let d = D.rename_symbols map t.d in
    remap e d

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
    let cnstr = List.fold_left (fun cnstr (a,b) -> L.And(cnstr,L.Eq(L.Var a,L.Var b))) L.True (q.L.get_eqs ()) in
    constrain cnstr t

  let pp_debug pp_sym ff t =
    Format.fprintf ff "@[<v -7>";
    Format.fprintf ff "@[<hv 2>eqs:@ %a@]@," (L.pp pp_sym) (serialize_eq t L.True);
    Format.fprintf ff "@[<h>dom:@ %a@]@," (D.pp_print pp_sym) t.d;
    Format.fprintf ff "@[<h>sym:@ %a@]" (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int) (SSet.elements t.s);
    Format.fprintf ff "@]"

  let pp_print pp_sym ff t =
    begin match serialize_eq t L.True with
    | L.True -> ()
    | eqs ->
      Format.fprintf ff "%a ∧ " (L.pp pp_sym) eqs
    end;
    D.pp_print pp_sym ff t.d
end

