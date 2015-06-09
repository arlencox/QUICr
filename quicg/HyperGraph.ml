module Make
    (VT: HyperGraph_sig.Comparable_Printable)
    (SL: HyperGraph_sig.Comparable_Printable)
    (DL: HyperGraph_sig.Comparable_Printable)
    (L: HyperGraph_sig.Comparable_Printable) :
  (HyperGraph_sig.HyperGraph
   with type vtx_t = VT.t
    and type src_lbl_t = SL.t
    and type dst_lbl_t = DL.t
    and type lbl_t = L.t) =
struct
  module V = VT
  type vtx_t = VT.t
  type src_lbl_t = SL.t
  type dst_lbl_t = DL.t
  type lbl_t = L.t

  module VMap = Mapx.Make(V)
  module VSet = Setx.Make(V)

  module E =
  struct
    type t = {
      src: SL.t VMap.t;
      dst: DL.t VMap.t;
      label: L.t;
    }

    let compare a b =
      let src_res = VMap.compare SL.compare a.src b.src in
      if src_res = 0 then
        let dst_res = VMap.compare DL.compare a.dst b.dst in
        if dst_res = 0 then
          L.compare a.label b.label
        else
          dst_res
      else
        src_res

    let default_print_vtx ff (vtx,_lbl) =
      Format.fprintf ff "%a" V.pp_print vtx

    let default_vtx_sep ff () =
      Format.fprintf ff ", "

    let default_vtx_wrapl ff () =
      Format.fprintf ff "{"

    let default_vtx_wrapr ff () =
      Format.fprintf ff "}"

    let default_edge_draw ff (fmt_src,fmt_dst,_lbl) =
      Format.fprintf ff "%a --> %a" fmt_src () fmt_dst ()

    let pp_print
        ?print_src_vtx:(print_src_vtx=default_print_vtx)
        ?print_dst_vtx:(print_dst_vtx=default_print_vtx)
        ?src_vtx_sep:(src_vtx_sep=default_vtx_sep)
        ?dst_vtx_sep:(dst_vtx_sep=default_vtx_sep)
        ?vtx_wrapl:(vtx_wrapl=default_vtx_wrapl)
        ?vtx_wrapr:(vtx_wrapr=default_vtx_wrapr)
        ?edge_draw:(edge_draw=default_edge_draw)
        ()
        ff e =
      let fmt_vtx_set fmt_vtx_sep fmt_vtx ff set =
        vtx_wrapl ff ();
        let first = ref true in
        VMap.iter (fun vtx lbl ->
            if !first then
              first := false
            else
              fmt_vtx_sep ff ();
            fmt_vtx ff (vtx,lbl)
          ) set;
        vtx_wrapr ff ()
      in
      let fmt_src ff () = fmt_vtx_set src_vtx_sep print_src_vtx ff e.src in
      let fmt_dst ff () = fmt_vtx_set dst_vtx_sep print_dst_vtx ff e.dst in
      edge_draw ff (fmt_src,fmt_dst,e.label)

    let get_src_vtxs e =
      let res = VMap.fold (fun vtx _lbl list ->
          vtx::list
        ) e.src [] in
      List.rev res

    let get_dst_vtxs e =
      let res = VMap.fold (fun vtx _lbl list ->
          vtx::list
        ) e.dst [] in
      List.rev res

    let get_srcs e =
      let res = VMap.fold (fun vtx lbl list ->
          (vtx,lbl)::list
        ) e.src [] in
      List.rev res

    let get_dsts e =
      let res = VMap.fold (fun vtx lbl list ->
          (vtx,lbl)::list
        ) e.dst [] in
      List.rev res

    let get_vtxs e =
      let res = VMap.fold (fun vtx lbl set ->
          VSet.add vtx set
        ) e.src VSet.empty in
      let res = VMap.fold (fun vtx lbl set ->
          VSet.add vtx set
        ) e.dst res in
      res

    let get_label e = e.label

    let iter_srcs f e =
      VMap.iter f e.src

    let iter_dsts f e =
      VMap.iter f e.dst

    let fold_srcs f e r =
      VMap.fold f e.src r

    let fold_dsts f e r =
      VMap.fold f e.dst r

    let choose_src e =
      fst (VMap.choose e.src)

    let choose_dst e =
      fst (VMap.choose e.dst)

    let replace_vtx e vo vn =
      let src' =
        try
          let lbl = VMap.find vo e.src in
          let src' = VMap.remove vo e.src in
          VMap.add vn lbl src'
        with Not_found ->
          e.src
      in
      let dst' =
        try
          let lbl = VMap.find vo e.dst in
          let dst' = VMap.remove vo e.dst in
          VMap.add vn lbl dst'
        with Not_found ->
          e.dst
      in
      {e with
       src = src';
       dst = dst';
      }


    let create srcs dsts lbl =
      let srcs = List.fold_left (fun srcs (src,src_lbl) ->
          VMap.add src src_lbl srcs) VMap.empty srcs in
      let dsts = List.fold_left (fun dsts (dst,dst_lbl) ->
          VMap.add dst dst_lbl dsts) VMap.empty dsts in
      {
        src = srcs;
        dst = dsts;
        label = lbl;
      }
  end
  type edge_t = E.t

  module EMap = Mapx.Make(E)
  module ESet = Setx.Make(E)


  type t = {
    edges: edge_t EMap.t;
    succ: ESet.t VMap.t;
    pred: ESet.t VMap.t;
  }

  let iter_vtxs f g =
    VMap.iter (fun v _edges ->
        (*assert(VMap.mem v g.pred);*)
        f v
      ) g.succ

  let iter_edges f g =
    EMap.iter (fun e _e -> f e) g.edges

  let iter_succ f g vtx =
    try
      let s = VMap.find vtx g.succ in
      ESet.iter f s
    with Not_found ->
      ()
      (*raise (Invalid_argument "iter_succ: Vertex not in graph")*)

  let iter_pred f g vtx =
    try
      let s = VMap.find vtx g.pred in
      ESet.iter f s
    with Not_found ->
      ()
      (*raise (Invalid_argument "iter_pred: Vertex not in graph")*)

        (* formatting *)

  let pp_debug
      ?fmt_vtx:(fmt_vtx=V.pp_print)
      ?fmt_src_lbl:(fmt_src_lbl=SL.pp_print)
      ?fmt_dst_lbl:(fmt_dst_lbl=DL.pp_print)
      ?fmt_edge_lbl:(fmt_edge_lbl=L.pp_print) () ff g =
    let pp_edge ff edge =
      let pp_vertex pp_lbl ff (vtx,lbl) =
        Format.fprintf ff "%a:%a" fmt_vtx vtx pp_lbl lbl
      in
      let pp_edge ff (fmt_src, fmt_dst, lbl) =
        Format.fprintf ff "%a --> %a: %a" fmt_src () fmt_dst () fmt_edge_lbl lbl
      in
      E.pp_print
        ~print_src_vtx:(pp_vertex fmt_src_lbl)
        ~print_dst_vtx:(pp_vertex fmt_dst_lbl)
        ~edge_draw:pp_edge
        ()
        ff edge
    in
    Format.fprintf ff "edges@.";
    let count = ref 0 in
    iter_edges (fun edge ->
        incr count;
        Format.fprintf ff " %4d. %a@." !count pp_edge edge
      ) g;
    Format.fprintf ff "vertexes:@.";
    let count = ref 0 in
    iter_vtxs (fun v ->
        incr count;
        Format.fprintf ff " %4d. %a@." !count fmt_vtx v;
        Format.fprintf ff "      succ:@.";
        iter_succ (fun edge ->
            Format.fprintf ff "        %a@." pp_edge edge;
          ) g v;
        Format.fprintf ff "      pred:@.";
        iter_pred (fun edge ->
            Format.fprintf ff "        %a@." pp_edge edge;
          ) g v;
      ) g

      (* Structure Checking *)

  exception Invalid

  let check_edge (g:t) (edge:E.t) =
    E.iter_srcs (fun vtx _lbl ->
        let s = VMap.find vtx g.succ in
        if not (ESet.mem edge s) then
          raise Invalid
      ) edge;
    E.iter_dsts (fun vtx _lbl ->
        let s =
          try VMap.find vtx g.pred
          with Not_found ->
            Format.printf "Couldn't find vertex %a@." V.pp_print vtx;
            Format.printf "%a@." (pp_debug ()) g;
            raise Invalid
        in
        if not (ESet.mem edge s) then
          raise Invalid
      ) edge;
    try
      let e = EMap.find edge g.edges in
      if (E.compare e edge) <> 0 then
        raise Invalid
    with Not_found ->
      raise Invalid

  let check g =
    try
      VMap.iter (fun vtx edges ->
          ESet.iter (fun edge ->
              check_edge g edge
            ) edges
        ) g.succ;
      VMap.iter (fun vtx edges ->
          ESet.iter (fun edge ->
              check_edge g edge
            ) edges
        ) g.pred;
      EMap.iter (fun edge _edge ->
          check_edge g edge
        ) g.edges;
      true
    with Invalid ->
      false


        (* Edge routines *)

  let add_edge g edge =
    let succ = E.fold_srcs (fun vtx _lbl succ ->
        try
          let s = VMap.find vtx succ in
          (*assert(not (ESet.mem edge s));*)
          let s = ESet.add edge s in
          VMap.add vtx s succ
        with Not_found ->
          raise (Invalid_argument "add_edge: Vertex in src does not exist")
      ) edge g.succ in
    let pred = E.fold_dsts (fun vtx _lbl pred ->
        try
          let s = VMap.find vtx pred in
          (*assert(not (ESet.mem edge s));*)
          let s = ESet.add edge s in
          VMap.add vtx s pred
        with Not_found ->
          raise (Invalid_argument "add_edge: Vertex in dst does not exist")
      ) edge g.pred in
    let g = {
      succ = succ;
      pred = pred;
      edges = EMap.add edge edge g.edges;
    } in
    (*assert(check g);*)
    g

  let mem_edge g edge =
    (*assert(check g);*)
    EMap.mem edge g.edges

  let remove_edge g edge =
    if not (mem_edge g edge) then
      raise (Invalid_argument "remove_edge: Edge not in graph");
    let succ = E.fold_srcs (fun vtx _lbl succ ->
        try
          let s = VMap.find vtx succ in
          (*assert(ESet.mem edge s);*)
          let s = ESet.remove edge s in
          VMap.add vtx s succ
        with Not_found ->
          raise (Invalid_argument "remove_edge: Vertex in src does not exist")
      ) edge g.succ in
    let pred = E.fold_dsts (fun vtx _lbl pred ->
        try
          let s = VMap.find vtx pred in
          (*assert(ESet.mem edge s);*)
          let s = ESet.remove edge s in
          VMap.add vtx s pred
        with Not_found ->
          raise (Invalid_argument "remove_edge: Vertex in dst does not exist")
      ) edge g.pred in
    let g = {
      succ = succ;
      pred = pred;
      edges = EMap.remove edge g.edges;
    } in
    (*assert(check g);*)
    g




  let fold_succ f g vtx r =
    try
      let s = VMap.find vtx g.succ in
      ESet.fold f s r
    with Not_found ->
      r
  (*raise (Invalid_argument "fold_succ: Vertex not in graph")*)


  let fold_pred f g vtx r =
    try
      let s = VMap.find vtx g.pred in
      ESet.fold f s r
    with Not_found ->
      r
  (*raise (Invalid_argument "fold_pred: Vertex not in graph")*)


  let fold_edges f g r =
    EMap.fold (fun e _e r -> f e r) g.edges r


      (* Vertex routines *)

  let add_vtx g vtx =
    if VMap.mem vtx g.succ then
      g
        (*raise (Invalid_argument "add_vtx: Vertex already exists");*)
    else begin
      (*assert(not (VMap.mem vtx g.succ));*)
      (*assert(not (VMap.mem vtx g.pred);*)
      let result = {
        succ = VMap.add vtx ESet.empty g.succ;
        pred = VMap.add vtx ESet.empty g.pred;
        edges = g.edges;
      } in
      (*assert(check result);*)
      result
    end

  let mem_vtx g vtx =
    let res = VMap.mem vtx g.succ in
    (*assert(res = (VMap.mem vtx g.pred));*)
    res


  let fold_vtxs f g r =
    VMap.fold (fun v _edges r ->
        (*assert(VMap.mem v g.pred);*)
        f v r
      ) g.succ r

  let remove_vtx g vtx =
    if not (VMap.mem vtx g.succ) then
      g
        (*raise (Invalid_argument "remove_vtx: Vertex does not exist");*)
    else begin
      (*assert(VMap.mem vtx g.succ);*)
      (*assert(VMap.mem vtx g.pred);*)
      let g = fold_succ (fun edge g ->
          remove_edge g edge
        ) g vtx g in
      let g = fold_pred (fun edge g ->
          remove_edge g edge
        ) g vtx g in
      let g = {
        succ = VMap.remove vtx g.succ;
        pred = VMap.remove vtx g.pred;
        edges = g.edges;
      } in
      (*assert(check g);*)
      g
    end

  let replace_vtx g vo vn =
    (*if not (VMap.mem vo g.succ) then*)
    (*raise (Invalid_argument "replace_vtx: Source vertex does not exist");*)
    (*if (VMap.mem vn g.succ) then*)
    (*raise (Invalid_argument "replace_vtx: Target vertex already exists");*)
    let g = add_vtx g vn in
    let g = fold_succ (fun edge g ->
        let edge' = E.replace_vtx edge vo vn in
        add_edge g edge'
      ) g vo g in
    let g = fold_pred (fun edge g ->
        let edge' = E.replace_vtx edge vo vn in
        if mem_edge g edge' then
          g
        else
          add_edge g edge'
      ) g vo g in
    let g = remove_vtx g vo in
    (*assert(check g);*)
    g



  let empty =
    let g = {
      succ = VMap.empty;
      pred = VMap.empty;
      edges = EMap.empty;
    } in
    (*assert(check g);*)
    g

      (* Basic Queries *)

  let is_empty g =
    let res = VMap.is_empty g.succ in
    (*assert(res = (VMap.is_empty g.pred));*)
    res

  let max_vtx g =
    let res = fst(VMap.max_binding g.succ) in
    (*assert(res = fst (VMap.max_binding g.pred));*)
    res

  let count_vtxs g =
    let res = VMap.cardinal g.succ in
    (*assert(res = (VMap.cardinal g.pred));*)
    res

  let count_edges g =
    EMap.cardinal g.edges

  let get_edge g edge =
    EMap.find edge g.edges




  let tarjans g =
    let index = ref 0 in
    let indexes = ref VMap.empty in
    let sccs = ref [] in
    let stack = Stack.create () in
    let set = ref VSet.empty in
    let rec scc n =
      indexes := VMap.add n (!index,!index) !indexes;
      incr index;
      Stack.push n stack;
      set := VSet.add n !set;

      iter_succ (fun edge ->
          let srcs = E.get_src_vtxs edge in
          let dsts = E.get_dst_vtxs edge in
          match srcs,dsts with
          | [], _ -> assert false
          | _, [] -> assert false
          | [src],[dst] ->
            (*assert((V.compare src n) = 0);*)
            if not (VMap.mem dst !indexes) then begin
              scc dst;
              let (src_index,src_lowlink) = VMap.find src !indexes in
              let (dst_index,dst_lowlink) = VMap.find dst !indexes in
              indexes := VMap.add src (src_index, min src_lowlink dst_lowlink) !indexes
            end 
            else if VSet.mem dst !set then
              let (src_index,src_lowlink) = VMap.find src !indexes in
              let (dst_index,dst_lowlink) = VMap.find dst !indexes in
              indexes := VMap.add src (src_index, min src_lowlink dst_index) !indexes
          | _ -> ()
        ) g n;
      let (src_index,src_lowlink) = VMap.find n !indexes in
      if src_index = src_lowlink then begin
        let scc = ref [] in
        let continue = ref true in
        while !continue do
          let w = Stack.pop stack in
          set := VSet.remove w !set;
          scc := w :: !scc;
          if (V.compare w n) = 0 then
            continue := false
        done;
        sccs := !scc :: !sccs
      end
    in
    iter_vtxs (fun vtx ->
        if not (VMap.mem vtx !indexes) then
          scc vtx
      ) g;
    !sccs

  let sccs g =
    let (g,multi_edges) = fold_edges (fun edge (g,multi_edges) ->
        let srcs = E.get_srcs edge in
        let dsts = E.get_dsts edge in
        let lbl = E.get_label edge in
        match srcs,dsts with
        | [], _ -> assert false
        | _, [] -> assert false
        | [src],[dst] -> (g, multi_edges)
        | srcs,dsts ->
          (* construct product edges of srcs and dsts *)
          let (g,added_edges) = List.fold_left (fun (g,added_edges) src ->
              List.fold_left (fun (g,added_edges) dst ->
                  let new_edge = E.create [src] [dst] lbl in
                  if mem_edge g new_edge then
                    (g,added_edges)
                  else
                    (add_edge g new_edge, ESet.add new_edge added_edges)
                ) (g,added_edges) dsts
            ) (g,ESet.empty) srcs in
          let multi_edges = EMap.add edge added_edges multi_edges in
          (g, multi_edges)
          (*match edge with
            | Inclusion _ -> (g, multi_edges)
            | Union(src,dst1,dst2) ->
                let (g,added_edges) = if mem_inclusion g src dst2 then
                    (g,[]) else
                    (add_inclusion g src dst2 lbl, [(src,dst2)])
                in
                let (g,added_edges) = if mem_inclusion g src dst1 then
                    (g,added_edges) else
                    (add_inclusion g src dst1 lbl, (src,dst1)::added_edges)
                in
                let multi_edges = if added_edges <> [] then
                    EMap.add edge added_edges multi_edges
                  else
                    multi_edges
                in
                (g,multi_edges)
            | Intersection(src1,src2,dst) ->
                let (g,added_edges) = if mem_inclusion g src2 dst then
                    (g,[]) else
                    (add_inclusion g src2 dst lbl, [(src2,dst)])
                in
                let (g,added_edges) = if mem_inclusion g src1 dst then
                    (g,added_edges) else
                    (add_inclusion g src1 dst lbl, (src1,dst)::added_edges)
                in
                let multi_edges = if added_edges <> [] then
                    EMap.add edge added_edges multi_edges
                  else
                    multi_edges
                in
                (g,multi_edges)*)
      ) g (g,EMap.empty)
    in
    let rec run_tj g multi_edges =
      (*Format.printf "Starting iteration@.";
        Format.printf "%a@." (fmt_debug N.print (fun ff a -> ())) g;*)
      let sccs = tarjans g in
      (*Format.printf "Internal SCCs:@.";
        List.iter (fun scc ->
        Format.printf "%a@." (Listx.fmt_str_sep ", " N.print) scc;
        ) sccs;*)
      (* construct a map from each node to its scc id *)
      let (_i,sccids) = List.fold_left (fun (i,sccids) scc ->
          (i+1,List.fold_left (fun sccids el ->
               VMap.add el i sccids
             ) sccids scc)
        ) (0,VMap.empty) sccs in
      (* iterate over the multi-edges. Check if all nodes from a multi-edge are within a single scc,
         if not, delete edge from multi-edges and delete added edges and rerun tarjans on new graph *)
      let (rerun, g, multi_edges) = EMap.fold (fun edge added_edges (rerun, g, multi_edges) ->
          let src_vtxs = E.get_src_vtxs edge in
          let dst_vtxs = E.get_dst_vtxs edge in
          let src,srcs = match src_vtxs with
            | src::srcs -> src,srcs
            | _ -> assert false
          in
          let dst,dsts = match dst_vtxs with
            | dst::dsts -> dst,dsts
            | _ -> assert false
          in
          let src_scc = VMap.find src sccids in
          let dst_scc = VMap.find dst sccids in
          if src_scc = dst_scc &&
             (List.for_all (fun src -> (VMap.find src sccids) = src_scc) srcs) &&
             (List.for_all (fun dst -> (VMap.find dst sccids) = dst_scc) dsts) then
            (rerun, g, multi_edges)
          else
            let g = ESet.fold (fun edge g ->
                (*Format.printf "Removing edge %a -> %a@." N.print src N.print dst;*)
                remove_edge g edge) added_edges g in
            let multi_edges = EMap.remove edge multi_edges in
            (true, g, multi_edges)


(*let (n1,n2,n3) = match edge with
          | Union(n1,n2,n3)
          | Intersection(n1,n2,n3) -> (n1,n2,n3)
          | _ -> assert false
        in
        let scc1 = VMap.find n1 sccids in
        let scc2 = VMap.find n2 sccids in
        let scc3 = VMap.find n3 sccids in
        (*Format.printf "Checking edge %a:%d %a:%d %a:%d@." N.print n1 scc1 N.print n2 scc2 N.print n3 scc3;*)
        if scc1 = scc2 && scc2 = scc3 then
          (rerun, g, multi_edges)
        else
          let g = List.fold_left (fun g (src,dst) ->
            (*Format.printf "Removing edge %a -> %a@." N.print src N.print dst;*)
            remove_inclusion g src dst) g added_edges in
          let multi_edges = EMap.remove edge multi_edges in
          (true, g, multi_edges)*)
        ) multi_edges (false, g, multi_edges) in
      if rerun then
        run_tj g multi_edges
      else
        sccs
    in
    run_tj g multi_edges
end

