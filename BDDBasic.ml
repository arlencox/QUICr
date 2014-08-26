
module Make
    (S : Interface.Sym)
    (C : Interface.Domain with type sym = S.t)
    (B : Interface.Domain with type sym = S.t)
  : Interface.Domain = struct
  type ctx = {
    cctx : C.ctx;
    bctx : B.ctx;
    man : Cudd.Man.d Cudd.Man.t;
  }

  type sym = S.t

  type expr = (sym, C.expr, C.cnstr) Interface.set_expr

  type cnstr = (sym, C.expr, C.cnstr) Interface.set_cnstr

  module Int = struct
    type t = int
    let compare = (-)
  end

  module IMap = Map.Make(Int)
  module SMap = Map.Make(S)
  module SSet = Set.Make(S)
  module CSet = Set.Make(C)

  module Base = struct
    type t = {
      uc: UC.t; (** underapproximation of cardinality *)
      oc: OC.t; (** overapproximation of cardinality *)
      ub: UB.t; (** underapproximation of contents *)
      ob: OB.t; (** overapproximation of contents *)
    }

    let top ctx = {
      uc = UC.bottom ctx.ucctx;
      oc = OC.top ctx.occtx;
      ub = UB.bottom ctx.ubctx;
      ob = OB.top ctx.obctx;
    }

    let empty = {
      uc = UC.constant ctx.ucctx 0;
      oc = OC.constant ctx.occtx 0;
      ub = UB.constant ctx.ubctx [];
      ob = UB.constant ctx.obctx [];
    }

    let universe = {

    let universe = top

    let singleton e =
      let c = C.of_expr e in
      {
        lb = CSet.singleton c;
        ub = Some (CSet.singleton c);
        lc = 1;
        uc = Some 1;
      }

    let union ia ib =
      let lb = CSet.union ia.lb ib.lb in
      {
        lb = lb;
        ub = (match ia.ub, ib.ub with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> Some (CSet.union ia ib));
        lc = max (min ia.lc ib.lc) (CSet.cardinal lb);
        uc =  (match ia.uc, ib.uc with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> Some (max ia ib));
      }

    let intersection ia ib =
      let lb = CSet.inter ia.lb ib.lb in
      {
        lb = lb;
        ub = (match ia.ub, ib.ub with
            | None, None -> None
            | None, Some o
            | Some o, None -> Some o
            | Some ia, Some ib -> Some (CSet.inter ia ib)
          );
        lc = CSet.cardinal lb;
        uc = (match ia.uc, ib.uc with
            | None, None -> None
            | None, Some o
            | Some o, None -> Some o
            | Some ia, Some ib -> Some (min ia ib)
          );
      }

    let complement i =
      top


    let join ia ib =
      {
        lb = CSet.inter ia.lb ib.lb;
        ub = (match ia.ub, ib.ub with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> Some (CSet.union ia ib));
        lc = min ia.lc ib.lc;
        uc = (match ia.uc, ib.uc with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> Some (max ia ib));
      }

    let widening ia ib =
      {
        lb = CSet.inter ia.lb ib.lb;
        ub = (match ia.ub, ib.ub with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> 
              if CSet.subset ib ia then
                Some ia
              else
                None
          );
        lc = min ia.lc ib.lc;
        uc = (match ia.uc, ib.uc with
            | None, _
            | _, None -> None
            | Some ia, Some ib -> if ib > ia then None
              else Some ia
          );
      }
  end

  type info = {
    bddid: int;   (** bdd identifier for this symbol *)
    base: Base.t; (** base domain for values and cardinality *)
  }

  type t = {
    ctx: ctx;
    bdd: Cudd.Man.d Cudd.Bdd.t;
    smap: info SMap.t;
    free: Fresh.t;
  }

  let init () = {
    bctx = C.init ();
    man = Cudd.Man.make_d ();
  }

  let top ctx syms =
    let (free,smap) = List.fold_left (fun (free,smap) sym ->
        let (free, id) = Fresh.fresh free in
        let info = {
          bddid = id;
          base = Base.top;
        } in
        (free, SMap.add sym info smap)
      ) (Fresh.empty,SMap.empty) syms in
    {
      ctx = ctx;
      bdd = Cudd.Bdd.dtrue ctx.man;
      smap = smap;
      free = free;
    }

  let topd t =
    {t with
     bdd = Cudd.Bdd.dtrue t.ctx.man }

  let bottomd t =
    {t with
     bdd = Cudd.Bdd.dfalse t.ctx.man }


  let bottom ctx syms =
    bottomd @@ top ctx syms

  let context t = t.ctx

  let symbols_set t =
    SMap.fold (fun sym _ syms -> SSet.add sym syms) t.smap SSet.empty

  let symbols t =
    SMap.fold (fun sym _ syms -> sym::syms) t.smap []

  let reduce a = a

  type gen_imap_thread = {
    imapa: int IMap.t; (** maps bdd index in a to index in c *)
    freea: Fresh.t; (** free indexes in a *)
    bdda: Cudd.Man.d Cudd.Bdd.t; (** a bdd *)
    imapb: int IMap.t; (** maps bdd index in b to index in c *)
    freeb: Fresh.t; (** free indexes in b *)
    bddb: Cudd.Man.d Cudd.Bdd.t; (** b bdd *)
    rmap: info SMap.t; (** maps symbol in c to bdd index in c *)
    freer: Fresh.t; (** free indexes in c *)
  }

  let upper_bound join_base mapping a b =
    let generate_smap mapping a b =
      let threaded = {
        imapa = IMap.empty;
        freea = a.free;
        bdda = a.bdd;
        imapb = IMap.empty;
        freeb = b.free;
        bddb = b.bdd;
        rmap = SMap.empty;
        freer = Fresh.empty;
      } in
      let threaded = List.fold_left (fun threaded (sa,sb,sr) ->
          (* get indexes of symbols a and b *)
          let ia = SMap.find sa a.smap in
          let ib = SMap.find sb b.smap in
          (* find target id, use existing if present, otherwise, create fresh *)
          let (freer,bddidr) = try (threaded.freer,(SMap.find sr threaded.rmap).bddid) with Not_found -> Fresh.fresh threaded.freer in
          (* determine if ia has already been assigned in the BDD, and if so, create a fresh index *)
          let cnstr_eq ix imapx freex bddx =
            if IMap.mem ix.bddid imapx then
              let (freex, bddidx') = Fresh.fresh freex in
              let dvar = Cudd.Bdd.ithvar a.ctx.man ix.bddid in
              let dvar' = Cudd.Bdd.ithvar a.ctx.man bddidx' in
              let deq = Cudd.Bdd.eq dvar dvar' in
              let bddx = Cudd.Bdd.dand deq bddx in
              ({ix with bddid=bddidx'},bddx,freex)
            else
              (ix,bddx,freex)
          in
          let (ia,bdda,freea) = cnstr_eq ia threaded.imapa threaded.freea threaded.bdda in
          let (ib,bddb,freeb) = cnstr_eq ib threaded.imapb threaded.freeb threaded.bddb in
          (* extend imaps *)
          let imapa = IMap.add ia.bddid bddidr threaded.imapa in
          let imapb = IMap.add ib.bddid bddidr threaded.imapb in
          (* join ia with ib *)
          let ir = {
            bddid = bddidr;
            base = join_base ia.base ib.base;
          } in
          (* extend rmap *)
          let rmap = SMap.add sr ir threaded.rmap in
          { imapa; freea; bdda; imapb; freeb; bddb; rmap; freer; }
        ) threaded mapping in
      threaded
    in

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
    in


    (* contexts must be identical *)
    assert(a.ctx == b.ctx);
    (* perform reductions as appropriate *)
    let a = reduce a in
    let b = reduce b in
    (* compute bdd mapping *)
    let threaded = generate_smap mapping a b in
    let bdda = do_rename threaded.imapa threaded.bdda in
    let bddb = do_rename threaded.imapb threaded.bddb in
    let bddc = Cudd.Bdd.dor bdda bddb in
    {
      ctx = a.ctx;
      bdd = bddc;
      smap = threaded.rmap;
      free = threaded.freer;
    }

  let join mapping a b =
    upper_bound Base.join mapping a b

  let widening mapping a b =
    upper_bound Base.widening mapping a b

  let meet mapping a b =
    failwith "unimplemented"

  let le mapping a b =
    failwith "unimplemented"

  exception Var_not_found of S.t
  exception Unsupported_expression

  let rec constrain (cnstr: cnstr) a =
    let rec of_expr = function
      | `Union (l,r) ->
        let (bddl, sl, bl) = of_expr l in
        let (bddr, sr, br) = of_expr r in
        let bdd = match bddl, bddr with
          | Some bddl, Some bddr -> Some (Cudd.Bdd.dor bddl bddr)
          | _ -> None
        in
        let s = match sl, sr with
          | Some sl, Some sr when S.compare sl sr = 0 -> Some sl
          | _ -> None
        in
        let b = Base.union bl br in
        (bdd,s,b)
      | `Inter (l,r) ->
        let (bddl, sl, bl) = of_expr l in
        let (bddr, sr, br) = of_expr r in
        let bdd = match bddl, bddr with
          | Some bddl, Some bddr -> Some (Cudd.Bdd.dand bddl bddr)
          | _ -> None
        in
        let s = match sl, sr with
          | Some sl, Some sr when S.compare sl sr = 0 -> Some sl
          | _ -> None
        in
        let b = Base.intersection bl br in
        (bdd, s, b)
      | `Difference (l,r) ->
        failwith "unimplemented"
      | `Complement e ->
        failwith "unimplemented"
      | `Var v ->
        begin try
            let info = (SMap.find v a.smap) in
            let bdd = Cudd.Bdd.ithvar a.ctx.man info.bddid in
            (Some bdd, Some v, info.base)
          with Not_found ->
            raise (Var_not_found v)
        end
      | `Singleton e -> 
        (None, None, Base.singleton e)
      | `Empty ->
        (Some (Cudd.Bdd.dfalse a.ctx.man), None, Base.empty)
      | `Universe ->
        (Some (Cudd.Bdd.dtrue a.ctx.man), None, Base.universe)
      | `Comprehension _ ->
        raise Unsupported_expression
    in
    try
      match cnstr with
      | `And(l,r) ->
        let a = constrain l a in
        constrain r a
      | `Eq(l,r) ->
        failwith "unimplemented"
      | `SubsetEq(l,r) ->
        let (bddl, sl, vl) = of_expr l in
        let (bddr, sr, vr) = of_expr r in
        let bdd = match bddl, bddr with
          | Some bddl, Some bddr ->
            (* constrain the bdd part *)
            let bdd = Cudd.Bdd.dor (Cudd.Bdd.dnot bddl) bddr in
            Cudd.Bdd.dand a.bdd bdd
          | _ -> a.bdd
        in
        let smap = match sl with
          | Some sl ->
            let info = try SMap.find sl a.smap with Not_found -> assert false in
            (* do subset constraint *)
            failwith "unimplemented"
          | None -> a.smap
        in
        let smap = match sr with
          | Some sr ->
            let info = try SMap.find sr smap with Not_found -> assert false in
            (* do subset constraint *)
            failwith "unimplemented"
          | None -> smap
        in
        { a with
          bdd = bdd;
          smap = smap;
        }
      | `Base bc ->
        raise Unsupported_expression
    with Unsupported_expression ->
      a

  let sat t c =
    failwith "unimplemented"

  let constrain_eq s1 s2 a =
    failwith "unimplemented"

  let add_symbols syms t =
    failwith "unimplemented"

  let remove_symbols syms t =
    failwith "unimplemented"

  let forget syms t =
    (* only use symbols that are already in the domain *)
    let syms = List.fold_left (fun syms s -> SSet.add s syms) SSet.empty syms in
    let symsr = symbols_set t in
    let syms = SSet.inter syms symsr in
    (* remove and then readd all of the symbols *)
    let syms = SSet.elements syms in
    let t = remove_symbols syms t in
    add_symbols syms t

  let rename_symbols f t =
    failwith "unimplemented"

  let to_constraint t =
    failwith "unimplemented"

  let equalities t =
    failwith "unimplemented"

  let abstract ctx syms c =
    let t = top ctx syms in
    constrain c t

  let abstractd d c =
    let t = topd d in
    constrain c t
end
