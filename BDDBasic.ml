module Make
    (S : Interface.Sym)
    (C : Interface.Constant with type sym = S.t)
  : Interface.DomainSimp
    with type sym = S.t
     and type cnstr = (S.t,
                       S.t Commands.set_expr,
                       C.cnstr,
                       S.t Commands.num_cnstr) Commands.set_cnstr
  =
struct

  type ctx = Cudd.Man.d Cudd.Man.t

  type sym = S.t

  type num_cnstr = sym Commands.num_cnstr

  type set_expr = sym Commands.set_expr

  type set_cnstr = [
    | `Eq of set_expr * set_expr
    | `SubsetEq of set_expr * set_expr
  ]

  type cnstr = [
    | set_cnstr
    | `Cardinal of num_cnstr
    | `ForAll of sym * sym * C.cnstr
    | `And of cnstr * cnstr
    | `True
    | `False
  ]

  module Int = struct
    type t = int
    let compare = (-)
  end

  module IMap = Map.Make(Int)
  module SMap = Map.Make(S)
  module SSet = Set.Make(S)
  module CMap = Map.Make(C)

  type cs =
    | Const of C.t
    | Symbol of S.t

  module CS = struct
    type t = cs
    let compare a b =
      match a,b with
      | Symbol _, Const _ -> -1
      | Const _, Symbol _ -> 1
      | Symbol a, Symbol b -> S.compare a b
      | Const a, Const b -> C.compare a b
  end

  type t = {
    c2i: (int * sym) CMap.t;
    s2i: int SMap.t;
    i2cs: cs IMap.t;
    ctx: ctx;
    bdd: Cudd.Man.d Cudd.Bdd.t;
    free: Fresh.t;
    sing: SSet.t;
  }

  let init () = Cudd.Man.make_d ()

  let add_constant c bv t =
    let (free,id) = Fresh.fresh t.free in
    let c2i = CMap.add c (id,bv) t.c2i in
    let i2cs = IMap.add id (Const c) t.i2cs in
    ({t with c2i; i2cs; free}, id)

  let add_constant c bv t =
    try
      (t,fst (CMap.find c t.c2i))
    with Not_found ->
      add_constant c bv t



  (** {2 Join/Widening/Meet -- Ternary Operations } *)




  (** [constant_mapping is_widening a b] constructs a mapping for constants based
      on which constants are shared between [a] and [b]. *)
  let constant_mapping is_widening a b =
    (* imperative mapping *)
    let mapping = ref [] in
    let add_mapping (a: int) (b: int) (c: int) =
      mapping := (a,b,c) :: !mapping
    in

    (* imperative fresh varible generator *)
    let free = ref Fresh.empty in
    let fresh () =
      let (free',v) = Fresh.fresh !free in
      free := free';
      v
    in

    (* imperative mapping construction *)
    let c2i = ref CMap.empty in
    let i2cs = ref IMap.empty in
    let add_index c i bv =
      c2i := CMap.add c (i,bv) !c2i;
      i2cs := IMap.add i (Const c) !i2cs
    in

    (* make a and b references *)
    let a = ref a in
    let b = ref b in
    let add_constant c bv a =
      let (a', id) = add_constant c bv !a in
      a := a';
      id
    in

    (* iterate over the constants *)
    ignore(CMap.merge (fun c ai bi ->
        match ai, bi with
        | Some (ai,bv), Some (bi,_) ->
          let ci = fresh () in
          add_mapping ai bi ci;
          add_index c ci bv;
          None
        | Some (ai,bv), None ->
          if is_widening then
            None
          else begin
            let bi = add_constant c bv b in
            let ci = fresh () in
            (* add constant to b *)
            add_mapping ai bi ci;
            add_index c ci bv;
            None
          end
        | None, Some (bi,bv) ->
          if is_widening then
            None
          else begin
            let ai = add_constant c bv a in
            let ci = fresh () in
            (* add constant to b *)
            add_mapping ai bi ci;
            add_index c ci bv;
            None
          end
        | None, None -> None
      ) !a.c2i !b.c2i);
    (
      !a, (* a potentially updated a domain *)
      !b, (* a potentially updated b domain *)
      {   (* the initial c domain *)
        c2i = !c2i;
        s2i = SMap.empty;
        i2cs = !i2cs;
        ctx = !a.ctx;
        bdd = Cudd.Bdd.dtrue !a.ctx;
        free = !free;
        sing = SSet.empty;
      },
      !mapping (* the mapping in terms of bdd ids *)
    )

  (* function that performs a renaming on the bdd *)
  let do_rename imap bdd =
    let max = fst (IMap.max_binding imap) in
    let mapping = Array.make (max+1) (-1) in
    IMap.iter (fun fid tid ->
        mapping.(fid) <- tid
      ) imap;
    let supp = snd (Array.fold_left (fun (i,supp) tid ->
        let supp = if tid < 0 then
            Cudd.Bdd.dand (Cudd.Bdd.ithvar (Cudd.Bdd.manager bdd) i) supp
          else
            supp in
        (i+1, supp)
      ) (0, Cudd.Bdd.dtrue (Cudd.Bdd.manager bdd)) mapping) in
    let bdd = Cudd.Bdd.exist supp bdd in
    Cudd.Bdd.permute bdd mapping

  let upper_bound op is_widening mapping a b =
    let (a,b,c,imapping) = constant_mapping is_widening a b in

    (* construct integer mapping for symbols *)
    let (imapping,c) = List.fold_left (fun (imapping,c) (va,vb,vc) ->
        let ia = SMap.find va a.s2i in
        let ib = SMap.find vb b.s2i in
        let (free,ic) = try
            (c.free,SMap.find vc c.s2i)
          with Not_found ->
            Fresh.fresh c.free
        in
        let s2i = SMap.add vc ic c.s2i in
        let i2cs = IMap.add ic (Symbol vc) c.i2cs in
        let c = {c with s2i; i2cs; free} in
        ((ia,ib,ic)::imapping, c)
      ) (imapping,c) mapping in

    (* construct a proper map *)
    let (amap,bmap) = List.fold_left (fun (amap,bmap) (ia,ib,ic) ->
        (IMap.add ia ic amap, IMap.add ib ic bmap)
      ) (IMap.empty,IMap.empty) imapping in

    (* do renaming *)
    let bdda = do_rename amap a.bdd in
    let bddb = do_rename bmap b.bdd in
    let bddc = op bdda bddb in
    {c with bdd = bddc}

  let meet = upper_bound Cudd.Bdd.dand false

  let join = upper_bound Cudd.Bdd.dor false

  let widening = upper_bound Cudd.Bdd.dor true



  (** {2 le -- binary operations} *)

  (** [constant_mapping is_widening a b] constructs a mapping for constants based
      on which constants are shared between [a] and [b]. *)
  let constant_le_mapping a b =
    (* imperative mapping *)
    let mapping = ref [] in
    let add_mapping a b =
      mapping := (a,b) :: !mapping
    in

    (* make a and b references *)
    let a = ref a in
    let b = ref b in
    let add_constant c bv a =
      let (a', id) = add_constant c bv !a in
      a := a';
      id
    in

    (* iterate over the constants *)
    ignore(CMap.merge (fun c ai bi ->
        match ai, bi with
        | Some (ai,_), Some (bi,_) ->
          add_mapping ai bi;
          None
        | Some (ai,bv), None ->
            let bi = add_constant c bv b in
            (* add constant to b *)
            add_mapping ai bi;
            None
        | None, Some (bi,bv) ->
            let ai = add_constant c bv a in
            (* add constant to b *)
            add_mapping ai bi;
            None
        | None, None -> None
      ) !a.c2i !b.c2i);
    (
      !a, (* a potentially updated a domain *)
      !b, (* a potentially updated b domain *)
      !mapping (* the mapping in terms of bdd ids *)
    )

  let le mapping a b =
    let (a,b,imapping) = constant_le_mapping a b in

    (* construct integer mapping for symbols *)
    let imapping = List.fold_left (fun imapping (va,vb) ->
        let ia = SMap.find va a.s2i in
        let ib = SMap.find vb b.s2i in
        ((ia,ib)::imapping)
      ) (imapping) mapping in

    (* construct a proper map *)
    let amap = List.fold_left (fun amap (ia,ib) ->
        IMap.add ia ib amap
      ) IMap.empty imapping in

    let bdda = do_rename amap a.bdd in
    let bddb = b.bdd in
    Cudd.Bdd.is_leq bdda bddb


  let top ctx syms =
    let (free,s2i,i2cs) = List.fold_left (fun (free,s2i,i2cs) s ->
        let (free, i) = Fresh.fresh free in
        let s2i = SMap.add s i s2i in
        let i2cs = IMap.add i (Symbol s) i2cs in
        (free,s2i,i2cs)
      ) (Fresh.empty,SMap.empty,IMap.empty) syms in
    {
      ctx = ctx;
      bdd = Cudd.Bdd.dtrue ctx;
      s2i = s2i;
      i2cs = i2cs;
      c2i = CMap.empty;
      free = free;
      sing = SSet.empty;
    }

  let bottom ctx syms =
    let t = top ctx syms in
    { t with bdd = Cudd.Bdd.dfalse ctx }

  let context t = t.ctx

  let symbols_set t =
    SMap.fold (fun sym _ syms -> SSet.add sym syms) t.s2i SSet.empty

  let symbols t =
    SMap.fold (fun sym _ syms -> sym::syms) t.s2i []

  exception Var_not_found of S.t

  let constrain_set (cnstr: set_cnstr) bdd t =
    let rec of_expr = function
      | `Union (l,r) ->
        Cudd.Bdd.dor (of_expr l) (of_expr r)
      | `Inter (l,r) ->
        Cudd.Bdd.dand (of_expr l) (of_expr r)
      | `Difference (l,r) ->
        Cudd.Bdd.dand (of_expr l) (Cudd.Bdd.dnot (of_expr r))
      | `Complement e ->
        Cudd.Bdd.dnot (of_expr e)
      | `Var v ->
        begin try
            Cudd.Bdd.ithvar t.ctx (SMap.find v t.s2i)
          with Not_found ->
            raise (Var_not_found v)
        end
      | `Empty ->
        Cudd.Bdd.dfalse t.ctx
      | `Universe ->
       Cudd.Bdd.dtrue t.ctx
    in
    match cnstr with
    | `Eq(l,r) ->
      let l = of_expr l in
      let r = of_expr r in
      let bdd' = Cudd.Bdd.eq l r in
      Cudd.Bdd.dand bdd bdd'
    | `SubsetEq(l,r) ->
      let l = of_expr l in
      let r = of_expr r in
      let bdd' = Cudd.Bdd.dor (Cudd.Bdd.dnot l) r in
      Cudd.Bdd.dand bdd' bdd'


  let rec constrain_cardinal (c: num_cnstr) t =
    match c with
    | `And (c1, c2) ->
      let t = constrain_cardinal c1 t in
      constrain_cardinal c2 t
    | `Eq (`Var v, `Const 1) ->
      {t with sing = SSet.add v t.sing }
    | `Eq (`Const 1, `Var v) ->
      {t with sing = SSet.add v t.sing }
    | _ ->
      t

  let rec constrain_forall bv sv c t =
    match C.constrain c with
    | Some c ->
      List.fold_left (fun t (v,cnst) ->
          if S.compare v bv = 0 then
            let (t,id) = add_constant cnst bv t in
            let id = Cudd.Bdd.ithvar t.ctx id in
            let sid = SMap.find sv t.s2i in
            let sid = Cudd.Bdd.ithvar t.ctx sid in
            let bdd' = Cudd.Bdd.eq sid id in
            { t with bdd = Cudd.Bdd.dand t.bdd bdd' }
          else
            t
        ) t c
    | None ->
      {t with bdd = Cudd.Bdd.dfalse t.ctx}


  let rec constrain (cnstr: cnstr) a =
      match cnstr with
      | #set_cnstr as cnstr ->
        { a with bdd = constrain_set cnstr a.bdd a }
      | `And(l,r) ->
        let a = constrain l a in
        constrain r a
      | `Cardinal c ->
        constrain_cardinal c a
      | `ForAll (bv, sv, c) ->
        constrain_forall bv sv c a
      | `True ->
        a
      | `False ->
        { a with bdd = Cudd.Bdd.dfalse a.ctx }


  let rec sat_cardinal t c =
    match c with
    | `And (c1, c2) ->
      sat_cardinal t c2 && sat_cardinal t c2
    | `Eq (`Var v, `Const 1)
    | `Eq (`Const 1, `Var v) ->
      SSet.mem v t.sing
    | _ ->
      false



  let sat_forall (bv: sym) (sv: sym) (c: C.cnstr) (t:t) : bool =
    match C.sat c with
    | Some c ->
      List.for_all (fun (v,cnst) ->
          if S.compare v bv = 0 then
            let (t,id) = add_constant cnst bv t in
            let id = Cudd.Bdd.ithvar t.ctx id in
            let sid = SMap.find sv t.s2i in
            let sid = Cudd.Bdd.ithvar t.ctx sid in
            let bdd' = Cudd.Bdd.eq sid id in
            Cudd.Bdd.is_leq t.bdd bdd'
          else
            false
        ) c
    | None ->
      false

  let sat (t: t) (c: cnstr) : bool =
    let bdd_of_bool b =
      if b then
        Cudd.Bdd.dtrue t.ctx
      else
        Cudd.Bdd.dfalse t.ctx
    in
    let rec sat bdd : cnstr -> 'a = function
      | #set_cnstr as cnstr ->
        constrain_set cnstr bdd t
      | `And(l,r) ->
        let bdd = sat bdd l in
        sat bdd r
      | `Cardinal c ->
        Cudd.Bdd.dand bdd (bdd_of_bool (sat_cardinal t c))
      | `ForAll (bv, sv, c) ->
        Cudd.Bdd.dand bdd (bdd_of_bool (sat_forall bv sv c t))
      | `True ->
        bdd
      | `False ->
        bdd_of_bool false
    in
    let bdd = sat (Cudd.Bdd.dtrue t.ctx) c in
    Cudd.Bdd.is_leq t.bdd bdd


  let constrain_eq s1 s2 a =
    constrain (`Eq (`Var s1, `Var s2)) a

  let add_symbol t sym =
    if SMap.mem sym t.s2i then
      t
    else
      let (free,id) = Fresh.fresh t.free in
      { t with
        s2i = SMap.add sym id t.s2i;
        i2cs = IMap.add id (Symbol sym) t.i2cs;
        free = free;
      }

  let add_symbols ?level:(lvl=0) syms t =
    assert(lvl = 0);
    List.fold_left add_symbol t syms

  let remove_symbols ?level:(lvl=0) syms t =
    assert(lvl = 0);
    let (t, support) = List.fold_left (fun (t, support) sym ->
        try
          let id = SMap.find sym t.s2i in
          let t = {t with 
           s2i = SMap.remove sym t.s2i;
           i2cs = IMap.remove id t.i2cs;
           sing = SSet.remove sym t.sing;
          } in
          let id = Cudd.Bdd.ithvar t.ctx id in
          (t, Cudd.Bdd.dand id support)
        with Not_found ->
          (t, support)
      ) (t, Cudd.Bdd.dtrue t.ctx) syms in
    {t with
     bdd = Cudd.Bdd.exist support t.bdd
    }

  let forget ?level:(lvl=0) syms t =
    (* only use symbols that are already in the domain *)
    let syms = List.fold_left (fun syms s -> SSet.add s syms) SSet.empty syms in
    let symsr = symbols_set t in
    let syms = SSet.inter syms symsr in
    (* remove and then readd all of the symbols *)
    let syms = SSet.elements syms in
    let t = remove_symbols ~level:lvl syms t in
    add_symbols ~level:lvl syms t

  let rename_symbols ?level:(lvl=0) f t =
    assert(lvl = 0);
    let map = List.fold_left (fun map (f,t) -> SMap.add f t map) SMap.empty f in

    let s2i = SMap.fold (fun s i s2i ->
        let s = try
          SMap.find s map
        with Not_found ->
          s
        in
        SMap.add s i s2i
      ) SMap.empty t.s2i in
    let i2cs = IMap.fold (fun i cs i2cs ->
        let cs = match cs with
        | Symbol s ->
          let s = try
              SMap.find s map
            with Not_found ->
              s
          in
          Symbol s
        | Const _ -> cs
        in
        IMap.add i cs i2cs
      ) IMap.empty t.i2cs in
    { t with s2i; i2cs }

  let iter_quic f t =
    let nbdd = Cudd.Bdd.dnot t.bdd in
    Cudd.Bdd.iter_prime (fun vars ->
        let (n,p,i) = Array.fold_left (fun (n,p,i) v ->
            let (n,p) = match v with
              | Cudd.Man.True ->
                (i::n,p)
              | Cudd.Man.False ->
                (n,i::p)
              | _ ->
                (n,p)
            in
            let i = i + 1 in
            (n,p,i)
          ) ([],[],0) vars in
        f n p
      ) nbdd nbdd

  let fold_quic f t r =
    let r = ref r in
    iter_quic (fun n p ->
        r := f n p !r
      ) t;
    !r

  let get_eq_ids t =
    (* split into binary quic constraints and other quic constraints *)
    let (bin,other) = fold_quic (fun n p (bin,other) ->
        match n,p with
        | [n],[p] -> ((n,p)::bin,other)
        | _ -> (bin,(n,p)::other)
      ) t ([],[]) in

    (* split into equality constraints and other quic constraints *)
    let cmpbin (a,b) (c,d) =
      let min1 = min a b in
      let min2 = min c d in
      let max1 = max a b in
      let max2 = max c d in
      let res = compare min1 min2 in
      if res <> 0 then res
      else
        let res = compare max1 max2 in
        if res <> 0 then res
        else
          let res = compare a c in
          if res <> 0 then res
          else
            compare b d
    in
    let bin = List.sort cmpbin bin in
    let rec identify_eq eqs other = function
      | (a,b)::(c,d)::rest when a = d && b = c ->
        identify_eq ((a,b)::eqs) other rest
      | (a,b)::rest ->
        identify_eq eqs (([a],[b])::other) rest
      | [] ->
        (eqs,other)
    in
    let (eqs,other) = identify_eq [] other bin in
    (eqs,other)

  module U = UnionFind.Make(CS)

  let list2bin op un l =
    let res = List.fold_left (fun res el ->
        match res with
        | None -> Some el
        | Some res -> Some (op res el)
      ) None l
    in
    match res with
    | None -> un
    | Some s -> s

  exception Unsupported_representation

  let to_constraint t : cnstr =
    (* get equalities *)
    let (eqs,other) = get_eq_ids t in
    (* add equalities to union find ensuring symbols are always the representative *)
    let uf = List.fold_left (fun uf (a,b) ->
        let csa = IMap.find a t.i2cs in
        let csb = IMap.find a t.i2cs in
        U.union csa csb uf
      ) U.empty eqs in

    (* convert a list of ids to symbols raising an error if the representative is not a symbol *)
    let convert_and_replace l =
      List.rev_map (fun id ->
          let cs = IMap.find id t.i2cs in
          match snd(U.get_representative cs uf) with
          | Symbol s -> s
          | Const _ -> raise Unsupported_representation
        ) l
    in

    (* convert other to constraints *)
    let cnstrs = List.rev_map (fun (n,p) ->
        let n = List.map (fun v -> `Var v) (convert_and_replace n) in
        let n = list2bin (fun a b -> `Inter(a,b)) `Universe n in
        let p = List.map (fun v -> `Var v) (convert_and_replace p) in
        let p = list2bin (fun a b -> `Union(a,b)) `Empty p in
        `SubsetEq (n,p)
      ) other in

    (* convert equalities to either const contraints or set constraints *)
    let (cnstrs,forall) = U.fold (fun k v (cnstrs,forall) ->
        if CS.compare k v = 0 then
          (cnstrs,forall)
        else
          match k,v with
          | Symbol s, Const c
          | Const c, Symbol s ->
            (cnstrs, (s,c)::forall)
          | Symbol s1, Symbol s2 ->
            let eq = `Eq(`Var s1, `Var s2) in
            (eq::cnstrs, forall)
          | Const c1, Const c2 ->
            raise Unsupported_representation
      ) uf (cnstrs,[]) in

    (* TODO: show constant constraints.  Need a way to generate symbols *)
    let cnstrs = List.fold_left (fun cnstrs (s,c) ->
        let bv = snd(CMap.find c t.c2i) in
        let c = C.to_constraint [(bv,c)] in
        let cnstr = `ForAll (bv, s, c) in
        cnstr::cnstrs
      ) cnstrs forall in

    let cnstrs = SSet.fold (fun s cnstrs ->
        let cnstr = `Cardinal (`Eq(`Var s, `Const 1)) in
        cnstr::cnstrs
      ) t.sing cnstrs in

    list2bin (fun a b -> `And(a,b)) `True cnstrs


  let equalities t =
    let eqs = fst(get_eq_ids t) in
    List.fold_left (fun eqs (a,b) ->
        match IMap.find a t.i2cs, IMap.find b t.i2cs with
        | Symbol a, Symbol b -> (a,b)::eqs
        | _ -> eqs
      ) [] eqs


end
