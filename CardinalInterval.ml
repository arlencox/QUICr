module Make
    (S : Interface.Sym)
    (C : Interface.Ordered)
  : Interface.Domain = struct
  type ctx = unit 

  type sym = S.t

  type expr = sym Interface.set_expr

  type cnstr = [
    | (sym,expr,sym Interface.string_cnstr,sym Interface.num_cnstr) Interface.set_cnstr
    | `And of cnstr * cnstr
    | `True
    | `False
  ]

  module SMap = Map.Make(S)
  module SSet = Set.Make(S)

  module CS = ConstSet.Make(C)
  module SI = SimpleInterval

  type nonrel = {
    const: CS.t;
    card: SI.t;
  }

  type t = nonrel SMap.t

  let init () = ()

  let top ctx syms =
    List.fold_left (fun t sym ->
        SMap.add sym {
          const = CS.top;
          card = SI.top;
        } t
      ) SMap.empty syms


  let bottom ctx syms =
    List.fold_left (fun t sym ->
        SMap.add sym {
          const = CS.top;
          card = SI.bottom;
        } t
      ) SMap.empty syms


  let context t = ()

  let symbols t =
    SMap.fold (fun sym _ syms -> sym::syms) t []

  let symbols_set t =
    SMap.fold (fun sym _ syms -> SSet.add sym syms) t SSet.empty

  let topd d =
    top () (symbols d)

  let bottomd d =
    bottom () (symbols d)

  let is_bottom t =
    SMap.exists (fun _ info ->
        CS.is_bottom info.const || SI.is_bottom info.card
      ) t

  let upper_bound join_info mapping a b =
    if is_bottom a then
      b
    else if is_bottom b then
      a
    else
      List.fold_left (fun c (av,bv,cv) ->
          let ai = SMap.find av a in
          let bi = SMap.find bv b in
          let ci = join_info ai bi in
          SMap.add cv ci c
        ) SMap.empty mapping

  let join mapping a b =
    upper_bound (fun ai bi ->
        {
          const = CS.join ai.const bi.const;
          card = SI.join ai.card bi.card;
        }) mapping a b

  let widening mapping a b =
    upper_bound (fun ai bi ->
        {
          const = CS.widening ai.const bi.const;
          card = SI.widening ai.card bi.card;
        }) mapping a b

  let meet mapping a b =
    List.fold_left (fun c (av,bv,cv) ->
        let ai = SMap.find av a in
        let bi = SMap.find bv b in
        let ci = {
          const = CS.meet ai.const bi.const;
          card = SI.meet ai.card bi.card;
        } in
        SMap.add cv ci c
      ) SMap.empty mapping

  let le mapping a b =
    if is_bottom a then true
    else if is_bottom b then false
    else
      let (res,brem) = List.fold_left (fun (res,brem) (av,bv) -> 
          let ai = SMap.find av a in
          let bi = SMap.find bv b in
          let res = res && (CS.le ai.const bi.const) && (SI.le ai.card bi.card) in
          (res,SMap.remove bv brem)
        ) (true,b) mapping in
      if SMap.is_empty brem then
        res
      else
        false

  exception ToBottom

  let rec split_constraints cnstrs = function
    | `True -> cnstrs
    | `False -> raise ToBottom
    | `And(a,b) ->
      let cnstrs = split_constraints cnstrs a in
      split_constraints cnstrs b
    | `SubsetEq (l,r) ->
      let (subset,card,cont) = cnstrs in
      ((l,r)::subset, card, cont)
    | `Cardinal c ->
      let (subset,card,cont) = cnstrs in
      (subset, c::card, cont)
    | `ForAll (bv, sv, c) ->
      let (subset,card,cont) = cnstrs in
      (subset, card, (bv, sv, c)::cont)

  let split_constraints c =
    split_constraints ([],[],[]) c

  let rec eval_card_expr e t =
    match e with
    | `Var v ->
      (SMap.find v t).card
    | `Const i ->
      SI.const i
    | _ ->
      failwith "unimplemented"

  let rec constrain_cardinal c t =
    match c with
    | `And(c1, c2) ->
      let t = constrain_cardinal c1 t in
      constrain_cardinal c2 t
    | `Eq(`Var v, e)
    | `Eq(e, `Var v) ->
      let vcst = eval_card_expr e t in
      let info = SMap.find v t in
      let info = {info with
                  card = SI.meet info.card vcst } in
      SMap.add v info t
    | _ ->
      failwith "unimplemented"

  let eval_cont_expr e t =
    failwith "unimplemented"

  let rec constrain_contents (bv,sv,c) t =
    match c with
    | `Eq(`Var v, e)
    | `Eq(e, `Var v) when v = bv ->
      let vcnt = eval_cont_expr e t in
      let info = SMap.find sv t in
      let info = {info with
                  const = CS.meet info.const vcnt } in
      SMap.add sv info t
    | `Eq _ -> failwith "unimplemented"
    | `Ne(`Var v, e)
    | `Ne(e, `Var v) when v = bv ->
      let vcnt = eval_cont_expr e t in
      let vcnt = CS.complement vcnt in
      let info = SMap.find sv t in
      let info = {info with
                  const = CS.meet info.const vcnt } in
      SMap.add sv info t
    | `Ne _ -> failwith "unimplemented"
    | `And (c1,c2) ->
      let t = constrain_contents (bv,sv,c1) t in
      constrain_contents (bv,sv,c2) t
    | `Or (c1,c2) ->
      let t1 = constrain_contents (bv,sv,c1) t in
      let t2 = constrain_contents (bv,sv,c2) t in
      let mapping = List.map (fun m -> (m,m,m)) (symbols t) in
      join mapping t1 t2
    | `False ->
      bottomd t
    | `True ->
      t

  let constrain_subset (l,r) t =
    failwith "unimplemented"

  let constrain c t =
    let (subset,card,cont) = split_constraints c in
    let t = List.fold_left (fun t card -> constrain_cardinal card t) t card in
    let t = List.fold_left (fun t cont -> constrain_contents cont t) t cont in
    let t = List.fold_left (fun t subset -> constrain_subset subset t) t subset in
    t

  let sat t c =
    failwith "unimplemented"

  let constrain_eq s1 s2 a =
    failwith "unimplemented"

  let add_symbols ?level:(level=0) syms t =
    if level = 0 then
      List.fold_left (fun t sym ->
          if SMap.mem sym t then t
          else
            SMap.add sym {
              const = CS.top;
              card = SI.top;
            } t
        ) t syms
    else
      t

  let remove_symbols ?level:(level=0) syms t =
    if level = 0 then
      List.fold_left (fun t sym -> SMap.remove sym t) t syms
    else
      t

  let forget ?level:(level=0) syms t =
    (* only use symbols that are already in the domain *)
    let syms = List.fold_left (fun syms s -> SSet.add s syms) SSet.empty syms in
    let symsr = symbols_set t in
    let syms = SSet.inter syms symsr in
    (* remove and then readd all of the symbols *)
    let syms = SSet.elements syms in
    let t = remove_symbols syms t in
    add_symbols syms t

  let rename_symbols ?level:(level=0) f t =
    if level = 0 then
      let res = List.fold_left (fun res (s,d) ->
          SMap.remove s res
        ) t f in
      List.fold_left (fun res (s,d) ->
          SMap.add d (SMap.find s t) res
        ) res f
    else
      t

  let to_constraint t =
    failwith "unimplemented"

  let equalities t =
    []

  let abstract ctx syms c =
    let t = top ctx syms in
    constrain c t

  let abstractd d c =
    let t = topd d in
    constrain c t
end
