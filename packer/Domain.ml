module L = LogicSymbolicSet

let debug = false

module Make (D: Interface.Domain
             with type sym = int
              and type cnstr = int L.t
              and type output = int L.t
              and type query = int L.q
            )
  : Interface.Domain
    with type ctx = D.ctx
     and type sym = D.sym
     and type cnstr = D.cnstr
     and type query = D.query
     and type output = D.output
= struct
  module S = struct
    type t = int
    let compare = (-)
  end

  module SymMap = Map.Make(S)
  (*module SymSets = Util.UnionFind.Make(S)*)
  module SymSets = Util.Naive.Make(S)
  module SymSet = Set.Make(S)

  type ctx = D.ctx
  type sym = D.sym
  type cnstr = D.cnstr
  type output = D.output


  type query = D.query

  type pack = {
    doms: D.t SymMap.t;
    pack: SymSets.t;
  }

  type t = 
    | Bottom of ctx
    | Top of ctx
    | Dom of pack

  let pp_print pp_sym ff t =
    match t with
    | Bottom ctx ->
      Format.fprintf ff "false"
    | Top ctx ->
      Format.fprintf ff "true"
    | Dom t ->
      let first = ref true in
      SymMap.iter (fun _ d ->
          if !first then
            first := false
          else
            Format.fprintf ff " ∧ ";
          D.pp_print pp_sym ff d
        ) t.doms

  let pp_debug pp_sym ff t =
    match t with
    | Bottom ctx ->
      Format.fprintf ff "false"
    | Top ctx ->
      Format.fprintf ff "true"
    | Dom t ->
      Format.fprintf ff "@[<v 0>@[<h>[%a]@]" (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_sym) (SymSets.reps t.pack);
      SymMap.iter (fun r d ->
          Format.fprintf ff "@,@[<v 2>%a: " pp_sym r;
          Format.fprintf ff "[@[<h>%a@]]" (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_sym) (SymSets.elements r t.pack);
          Format.fprintf ff "@,%a@]" (D.pp_print pp_sym) d;
          (*D.pp_print pp_sym ff d*)
        ) t.doms;
      Format.fprintf ff "@]"

  let set_of_list l =
    List.fold_left (fun rs r -> SymSet.add r rs) SymSet.empty l

  let pp_set ff s =
    Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int ff (SymSet.elements s)

  let check ?msg:msg p =
    let pack_reps = set_of_list (SymSets.reps p.pack) in
    let dom_reps = set_of_list (SymMap.bindings p.doms |> List.map fst) in
    (* check that pack and dom represent the same things *)
    if not @@ SymSet.equal pack_reps dom_reps then begin
      Format.printf "@.@.Failure: %s @[<h>[%a] != [%a]@]@.@." (match msg with None -> "" | Some m -> m) pp_set pack_reps pp_set dom_reps;
      assert false
    end;

    (* check that the symbols of dom are a subset of those in the corresponding pack *)
    SymMap.iter (fun r d ->
        let dom_syms = set_of_list (D.symbols d) in
        let pack_syms = set_of_list (SymSets.elements r p.pack) in
        if not @@ SymSet.subset dom_syms pack_syms then begin
          Format.printf "@.@.Failure: pack %d: %s @[<h>[%a] !<= [%a]@]@.@." r (match msg with None -> "" | Some m -> m) pp_set dom_syms pp_set pack_syms;
          assert false
        end
      ) p.doms

  let check_t ?msg:msg = function
    | Bottom ctx -> ()
    | Top ctx -> ()
    | Dom p ->
      match msg with
      | None -> check p
      | Some msg -> check ~msg:msg p

  let init = D.init

  let top ctx = Top ctx

  let bottom ctx = Bottom ctx

  let context = function
    | Top ctx
    | Bottom ctx -> ctx
    | Dom p ->
      D.context @@ snd @@ SymMap.choose p.doms

  let symbols = function
    | Top _ -> []
    | Bottom _ -> []
    | Dom p ->
      if debug then check ~msg:"symbols" p;
      SymSets.fold (fun s _ l -> s::l) p.pack []

  (* add symbol to pack manager.  It may or may not exist *)
  let add ctx s ({pack; doms} as p) =
    if SymSets.mem s pack then
      p
    else
      {
        pack = SymSets.union s s pack;
        doms = SymMap.add s (D.top ctx) doms;
      }

  (* combine packs for the representatives r1 and r2. r1 and r2 must exist *)
  let merge_reps r1 r2 {pack; doms} =
    if debug then begin
      assert(SymSets.mem r1 pack);
      assert(SymSets.mem r2 pack);
      assert(SymMap.mem r1 doms);
      assert(SymMap.mem r2 doms);
    end;
    let pack = SymSets.union r1 r2 pack in
    let r = SymSets.rep r1 pack in
    let d1 = SymMap.find r1 doms in
    let d2 = SymMap.find r2 doms in
    let doms = doms |>
               SymMap.remove r1 |>
               SymMap.remove r2 |>
               SymMap.add r (D.meet d1 d2) in
    {pack; doms}

  (* merge two present packs based on symbols sym1 and sym2.  sym1 and sym2 are
     expected to already be present in p *)
  let merge_packs sym1 sym2 ({pack; doms} as p) =
    if sym1 = sym2 then
      p
    else
      let r1 = SymSets.rep sym1 pack in
      let r2 = SymSets.rep sym2 pack in
      if r1 = r2 then
        p
      else
        (* representatives are different, merge them *)
        merge_reps r1 r2 p

  (* merge list of present packs.  Each sym must be present already in domain *)
  let rec merge_pack_list syms pack =
    match syms with
    | [] -> pack
    | [sym] -> pack
    | sym1::sym2::rest ->
      let pack = merge_packs sym1 sym2 pack in
      merge_pack_list (sym2::rest) pack

  let merge_pack_list ctx syms ({pack; doms} as p) =
    let add_to_pack h exc {pack; doms} =
      let r = SymSets.rep h pack in
      (* compute new pack, by adding all remaining elements *)
      let pack = List.fold_left (fun pack o -> SymSets.union h o pack) pack exc in
      (* representative may have changed, update *)
      let r' = SymSets.rep h pack in
      if r = r' then
        {pack; doms}
      else
        let d = try SymMap.find r doms with Not_found -> assert false in
        let doms = doms |>
                   SymMap.remove r |>
                   SymMap.add r' d in
        {pack; doms}
    in
    match List.partition (fun s -> SymSets.mem s pack) syms with
    | [], [] -> p (* nothing to do *)
    | [], h::exc
    | [h], exc ->
      (* there is a single present element, no need to merge first *)
      let p = add ctx h p in
      add_to_pack h exc p
    | (h::_ as inc), exc ->
      (* perform merge first *)
      let p = merge_pack_list inc p in
      add_to_pack h exc p


  let rec symbols_e = function
    | L.Empty -> SymSet.empty
    | L.Universe -> SymSet.empty
    | L.DisjUnion (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.Union (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.Inter (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.Diff (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.Comp a -> symbols_e a
    | L.Var v -> SymSet.singleton v
    | L.Sing v -> SymSet.singleton v

  let rec syms = function
    | L.Eq (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.SubEq (a,b) -> SymSet.union (symbols_e a) (symbols_e b)
    | L.In (a,b) -> SymSet.add a (symbols_e b)
    | L.And (a,b) -> SymSet.union (syms a) (syms b)
    | L.Not a -> syms a
    | L.True -> SymSet.empty
    | L.False -> SymSet.empty


  let constrain cnstr t =
    let do_constrain ctx p =
      if debug then check ~msg:"constrain" p;
      let syms = SymSet.elements (syms cnstr) in
      let p = merge_pack_list ctx syms p in
      if debug then assert (List.for_all (fun s -> SymSets.mem s p.pack) syms);
      let t = match syms with
      | [] ->
        if SymMap.is_empty p.doms then
          let d = D.top ctx in
          let d = D.constrain cnstr d in
          if D.is_bottom d then
            Bottom ctx
          else
            Top ctx
        else
          let doms = SymMap.fold (fun srep d doms ->
              SymMap.add srep (D.constrain cnstr d) doms
            ) p.doms SymMap.empty in
          Dom { p with doms }
      | sym::_ ->
        let doms = p.doms in
        let pack = p.pack in
        let srep = SymSets.rep sym pack in
        let d = try SymMap.find srep doms with Not_found -> D.top ctx in
        let d = D.constrain cnstr d in
        let doms = SymMap.add srep d doms in
        Dom { pack; doms }
      in
      if debug then check_t ~msg:"constrain end" t;
      t
    in
    match t with
    | Bottom _ -> t
    | Top ctx ->
      do_constrain ctx {pack=SymSets.empty; doms=SymMap.empty}
    | Dom p ->
      let ctx = D.context @@ snd @@ SymMap.choose p.doms in
      do_constrain ctx p
      
  exception IsBottom

  let rec conjoin = function
    | [] -> L.True
    | [h] -> h
    | h::t -> L.And(h,conjoin t)

  let serialize = function
    | Bottom _ -> L.False
    | Top _ -> L.True
    | Dom t ->
      if debug then check ~msg:"serialize" t;
      try
        let cs = SymMap.fold (fun _ d l ->
            if D.is_bottom d then
              raise IsBottom
            else
              let c = D.serialize d in
              c::l
          ) t.doms [] in
        conjoin cs
      with IsBottom ->
        L.True

  let is_bottom t =
    match t with
    | Bottom _ -> true
    | Top _ -> false
    | Dom p ->
      if debug then check ~msg:"is_bottom" p;
      SymMap.exists (fun r d ->
          D.is_bottom d
        ) p.doms

  let sat t cnstr =
    match t with
    | Bottom _ -> true
    | Top ctx -> D.sat (D.top ctx) cnstr
    | Dom p ->
      if debug then check ~msg:"sat" p;
      let syms = SymSet.elements (syms cnstr) in
      let ctx = SymMap.choose p.doms |> snd |> D.context in
      let p = merge_pack_list ctx syms p in
      match syms with
      | [] ->
        SymMap.exists (fun _ d ->
            D.sat d cnstr) p.doms
      | sym::_ ->
        let doms = p.doms in
        let pack = p.pack in
        let srep = SymSets.rep sym pack in
        let d = try SymMap.find srep doms with Not_found -> assert false in
        D.sat d cnstr

  let forget ctx sym t =
    match t with
    | Bottom _
    | Top _ -> t
    | Dom t ->
      if debug then check ~msg:"forget" t;
      let pack = t.pack in
      let doms = t.doms in
      (* forget symbol within pack *)
      let r = SymSets.rep sym pack in
      let doms = try SymMap.add r (SymMap.find r doms |> D.forget [sym]) doms with Not_found -> doms in
      (* modify the packs if necessary *)
      let (pack,res) = SymSets.remove sym pack in
      let doms = match res with
        | SymSets.NoRepresentative ->
          SymMap.remove sym doms
        | SymSets.SameRepresentative ->
          doms
        | SymSets.NewRepresentative sym' ->
          try
            let d = SymMap.find sym doms in
            let doms = SymMap.remove sym doms in
            SymMap.add sym' d doms
          with Not_found ->
            SymMap.remove sym doms
      in
      if SymMap.is_empty doms then
        Top ctx
      else begin
        let t = Dom { pack; doms } in
        if debug then check_t ~msg:"forget end" t;
        t
      end

  let forget syms t =
    let ctx = context t in
    List.fold_left (fun t sym -> forget ctx sym t) t syms

  let unify_packs ctx a b =
    if a.pack == b.pack then
      (a.pack, a.doms, b.doms)
    else
      let (pack,ad,bd) = SymSets.merge a.pack b.pack in
      let rec do_changes t ch =
        let doms = List.fold_left (fun doms (rfrom,rto) ->
            (* merge from into to and remove from *)
            let dfrom = try SymMap.find rfrom doms with Not_found -> D.top ctx in
            let dto = try SymMap.find rto doms with Not_found -> D.top ctx in
            let dto = D.meet dfrom dto in
            doms |>
            SymMap.remove rfrom |>
            SymMap.add rto dto
          ) t.doms ch in
        doms
      in
      (pack, do_changes a ad, do_changes b bd)

  let bound_packs op a b =
    let ctx = SymMap.choose a.doms |> snd |> D.context in
    let (pack,adoms,bdoms) = unify_packs ctx a b in
    Dom {
      pack;
      doms = SymMap.merge (fun k d1 d2 ->
          match d1,d2 with
          | Some d1, Some d2 -> Some (op d1 d2)
          | Some d1, None -> Some (op d1 (D.top ctx))
          | None, Some d2 -> Some (op (D.top ctx) d2)
          | _ -> assert false
        ) adoms bdoms;
    }

  let upper_bound op a b =
    match a, b with
    | Bottom _, o
    | o, Bottom _ -> o
    | (Top _ as o), _
    | _, (Top _ as o) -> o
    | Dom a, Dom b ->
      bound_packs op a b

  let lower_bound op a b =
    match a, b with
    | (Bottom _ as o), _
    | _, (Bottom _ as o) -> o
    | Top _, o
    | o, Top _ -> o
    | Dom a, Dom b ->
      bound_packs op a b


  let join = upper_bound D.join

  let widening = upper_bound D.widening

  let meet = lower_bound D.meet
    
  exception Not_le

  let le a b =
    let do_le ctx a b =
      let (_pack, adoms, bdoms) = unify_packs ctx a b in
      try
        ignore(SymMap.merge (fun k d1 d2 ->
            match d1, d2 with
            | Some d1, Some d2 ->
              if D.le d1 d2 then
                None
              else
                raise Not_le
            | _ -> assert false
          ) adoms bdoms);
        true
      with Not_le ->
        false
    in
    match a, b with
    | Bottom _, _ -> true
    | _, Top _ -> true
    | Top _, Bottom _ -> false
    | Top ctx, Dom p ->
      do_le ctx {pack=SymSets.empty; doms=SymMap.empty} p
    | Dom a, Dom b ->
      let ctx = SymMap.choose a.doms |> snd |> D.context in
      do_le ctx a b
    | Dom a, Bottom _ ->
      is_bottom (Dom a)


  let rename_symbol o n p =
    let r = SymSets.rep o p.pack in
    let (r',pack) = match SymSets.remove o p.pack with
      | pack, SymSets.NoRepresentative ->
        (* no other representative for this set *)
        let pack = SymSets.union n n pack in
        (n,pack)
      | pack, SymSets.NewRepresentative r ->
        (* there is another representative *)
        let pack = SymSets.union r n pack in
        let r' = SymSets.rep n pack in
        (r',pack)
      | pack, SymSets.SameRepresentative ->
        let pack = SymSets.union r n pack in
        let r' = SymSets.rep n pack in
        (r',pack)
    in
    let doms = p.doms in
    let d = SymMap.find r doms in
    let doms = SymMap.remove r doms in
    let d = D.rename_symbols [(o,n)] d in
    let doms = SymMap.add r' d doms in
    { doms; pack }


  let rename_symbols sym_map t =
    match t with
    | Bottom _
    | Top _ -> t
    | Dom p ->
      if debug then check ~msg:"rename_symbols" p;
      let p = List.fold_left (fun p (o,n) ->
          rename_symbol o n p
        ) p sym_map in
      if debug then check ~msg:"rename_symbols end" p;
      Dom p

  let query o =
    match o with
    | Top _
    | Bottom _ ->
      {
        L.get_eqs = (fun () -> []);
        L.get_eqs_sym = (fun s -> []);
      }
    | Dom t ->
      {
        L.get_eqs = (fun () ->
            SymMap.bindings t.doms |>
            List.map snd |>
            List.map D.query |>
            List.map (fun q -> q.L.get_eqs ()) |>
            List.flatten
          );
        L.get_eqs_sym = (fun s ->
            (* find pack *)
            let pack = SymSets.union s s t.pack in
            let r = SymSets.rep s pack in
            let q = D.query (SymMap.find r t.doms) in
            q.L.get_eqs_sym s
          );
      }

  let combine q t =
    let eqs = q.L.get_eqs () in
    List.fold_left (fun t (s1,s2) ->
        constrain (L.Eq (L.Var s1, L.Var s2)) t
      ) t eqs


end
