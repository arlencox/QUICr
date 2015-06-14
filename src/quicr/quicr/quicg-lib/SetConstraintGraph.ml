let hide_base = ref false

let split_conj = ref true
let args = [
  ("-hide-base", Arg.Set hide_base, "Hide printing of base domain facts");
  ("-no-split-conj", Arg.Clear split_conj, "Insert newlines after each conjunction")
]

module Make(ND: QGInterface.ScalarDomain) =
struct

  let args = []

  let pp_conj_gap escape ff () =
    if !split_conj then
      FormatHelp.fmt_newline escape ff ()
    else
      Format.fprintf ff " "
    
  type base_symbol = BaseSym.t

  type set_symbol = SetSym.t

  type vtx_t =
    | Empty
    | Singleton of base_symbol
(*    | Universe*)
    | Variable of set_symbol

  module V = struct
    type t = vtx_t

    let num_of_vtx = function
      | Empty -> 0
      (*| Universe -> 1*)
      | Variable _ -> 2
      | Singleton _ -> 3

    let compare a b =
      match a, b with
        | Empty, Empty -> 0
        | Singleton a, Singleton b -> compare a b
        | Variable a, Variable b -> compare a b
        (*| Universe, Universe -> 0*)
        | _ -> compare (num_of_vtx a) (num_of_vtx b)

    let pp_print ff = function
      | Empty -> FormatHelp.fmt_emptyset FormatHelp.Text ff ()
      | Singleton b ->
          Format.fprintf ff "{b%a}" BaseSym.fmt b
      | Variable s ->
          Format.fprintf ff "s%a" SetSym.fmt s
      (*| Universe ->
          FormatHelp.fmt_universeset FormatHelp.Text ff ()*)
  end

  module Unit = struct
    type t = unit
    let compare _a _b = 0
    let pp_print ff v = ()
  end

  module L = struct
    type t = ND.t

    let compare a b = 0

    let pp_print ff v =
      ()
  end

  module HG = HyperGraph.Make(V)(Unit)(Unit)(L)
  module G = Z3HyperGraphMatch.Make(HG)

  type ctx = ND.ctx

  type t = {
    g: G.t;
    based: BaseSym.t;
  }

  let pp_print ?escape:(escape=FormatHelp.Text) ctx fmt_scalar_var fmt_set_var ff t =
    let module FH = FormatHelp in
    let fmt_scalar_var ff bv =
      if bv = (BaseSym.to_int t.based) then
        Format.fprintf ff "%a" (FH.fmt_extra_var escape) ()
      else
        fmt_scalar_var ff bv
    in
    let fmt_scalar_varb ff x =
      let bv = BaseSym.to_int x in
      fmt_scalar_var ff bv
    in
    let fmt_set_var ff x = fmt_set_var ff (SetSym.to_int x) in
    let fmt_vertex ff (vtx,_lbl) =
      match vtx with
        | Empty -> FH.fmt_emptyset escape ff ()
        | Singleton base ->
            Format.fprintf ff "%a%a%a" (FH.fmt_leftbr escape) () fmt_scalar_varb base (FH.fmt_rightbr escape) ()
        | Variable set -> fmt_set_var ff set
        (*| Universe -> FH.fmt_universeset escape ff ()*)
    in
    let fmt_empty_wrap ff () = () in
    let fmt_edge ff (fmt_src,fmt_dst,lbl) =
      (
      if !hide_base || (ND.is_top ctx lbl) then
        Format.fprintf ff "%a %a %a"
          fmt_src ()
          (FH.fmt_subseteq escape) ()
          fmt_dst ()
      else
        Format.fprintf ff "%a %a %a"
          fmt_src ()
          (FH.fmt_subseteq escape) ()
          (FH.fmt_set_comp escape
             (fun ff () ->
               Format.fprintf ff "%a %a %a"
                 (FH.fmt_extra_var escape) ()
                 (FH.fmt_in escape) ()
                 fmt_dst ()
             )
             (fun ff () ->
               Format.fprintf ff "%a"
                 (ND.fmt ~escape:escape ctx fmt_scalar_var) lbl
             )
          ) ()
      )
      (*Format.fprintf ff ": %d" (ND.get_dim ctx lbl)*)
    in
    let first = ref true in
    G.iter_edges (fun edge ->
      if !first then
        first := false
      else
        Format.fprintf ff " %a%a" (FH.fmt_conj escape) () (pp_conj_gap escape) ();
      G.E.pp_print
        ~print_src_vtx:fmt_vertex
        ~print_dst_vtx:fmt_vertex
        ~src_vtx_sep:(fun ff () -> Format.fprintf ff " %a " (FH.fmt_inter escape) ())
        ~dst_vtx_sep:(fun ff () -> Format.fprintf ff " %a " (FH.fmt_union escape) ())
        ~vtx_wrapl:fmt_empty_wrap
        ~vtx_wrapr:fmt_empty_wrap
        ~edge_draw:fmt_edge
        ()
        ff edge
    ) t.g

  let fmt_vtx_list sep ff vl =
    Listx.fmt_str_sep sep V.pp_print ff vl

  let fmt_edge ff (srcs,dsts) =
    Format.fprintf ff "%a <= %a"
      (fmt_vtx_list " ^ ") srcs
      (fmt_vtx_list " U ") dsts

  let fmt_gedge ff edge =
    G.E.pp_print () ff edge

  let fmt_base ctx ff nd =
    ND.fmt ctx (fun ff b -> Format.fprintf ff "b%d" b) ff nd
      

  let empty ctx based =
    let g = G.empty in
    let g = G.add_vtx g Empty in
    (*let g = G.add_vtx g Universe in*)
    {g=g;
     based=based}

  let copy_atoms ctx s =
    let g = G.empty in
    let g = G.fold_vtxs (fun vtx g ->
      G.add_vtx g vtx
    ) s.g g in
    {g=g;
     based=s.based;}

   

  let add_variable ctx t v =
    {t with
      g = G.add_vtx t.g (Variable v)}

  let add_singleton ctx t b =
    {t with
      g = G.add_vtx t.g (Singleton b)}

  let add_atom ctx t a =
    {t with
      g = G.add_vtx t.g a}

  exception Short_circuit

  let is_empty ctx t =
    try
      G.iter_edges (fun _ ->
        raise Short_circuit
      ) t.g;
      true
    with Short_circuit ->
      false
    
  let simplify_union rhs =
    (*try*)
      let res = List.fold_right (fun el rhs ->
        match el with
          | Empty -> rhs
          (* | Universe -> raise Short_circuit*)
          | _ -> el::rhs
      ) rhs []
      in
      begin match res with
        | [] -> [Empty]
        | _ -> res
      end
    (*with Short_circuit ->
      [Universe]*)

  let simplify_intersection lhs =
    try
      let res = List.fold_right (fun el lhs ->
        match el with
          | Empty -> raise Short_circuit
          (*| Universe -> lhs*)
          | _ -> el::lhs
      ) lhs [] in
      begin match res with
        | [] -> assert false (*[Universe]*)
        | _ -> res
      end
    with Short_circuit ->
      [Empty]

  let add_constraint ctx t lhs rhs lbl =
    let lhs = simplify_intersection lhs in
    let rhs = simplify_union rhs in
    let lhss = G.VSet.of_list lhs in
    let rhss = G.VSet.of_list rhs in
    let inter = G.VSet.inter lhss rhss in
    (* introduce restriction *)
    if (((List.length lhs) = 1) || ((List.length rhs) = 1))
      && ((G.VSet.is_empty inter) || ((G.VSet.compare lhss rhss) = 0)) then begin
      (*Format.printf "Add_constraint: %a: %a@." fmt_edge (lhs,rhs) (fmt_vtx_list ", ") (G.VSet.elements inter);*)
      let convert_and_add vtx (t,lst) =
        let t = if G.mem_vtx t.g vtx then t
          else {t with g = G.add_vtx t.g vtx} in
        (t,(vtx,())::lst)
      in
      let (t,lhs) = List.fold_right convert_and_add lhs (t,[]) in
      let (t,rhs) = List.fold_right convert_and_add rhs (t,[]) in
      let e = G.E.create lhs rhs lbl in
      try
        let e_orig = G.get_edge t.g e in
        let new_lbl = ND.meet ctx (G.E.get_label e_orig) lbl in
        let e = G.E.create lhs rhs new_lbl in
        let g = G.remove_edge t.g e in
        {t with
          g = G.add_edge g e}
      with Not_found ->
        {t with
          g = G.add_edge t.g e}
    end else
      t

  (* project out a variable from the constraint graph *)
  let project_atom ctx t v =
    if not(G.mem_vtx t.g v) then
      t
      (*raise (Invalid_argument "project_atom: Can't project a non-existent atom");*)
    else begin
      let t = G.fold_pred (fun pred_e t ->
          let pred_srcs = G.E.get_src_vtxs pred_e in
          let pred_dsts = G.E.get_dst_vtxs pred_e in
          let pred_lbl = G.E.get_label pred_e in
          G.fold_succ (fun succ_e t ->
              let succ_srcs = G.E.get_src_vtxs succ_e in
              let succ_dsts = G.E.get_dst_vtxs succ_e in
              let succ_lbl = G.E.get_label succ_e in
              match pred_srcs,pred_dsts,succ_srcs,succ_dsts with
              | pred_srcs, [pred_dst], succ_srcs, [succ_dst] ->
                (* inclusion -> inclusion
                 * inclusion -> intersection
                 * intersection -> inclusion
                 * intersection -> intersection
                *)
                assert((V.compare pred_dst v) = 0);
                let succ_srcs = List.filter (fun ss -> (V.compare ss v) <> 0) succ_srcs in
                let new_lbl = ND.meet ctx pred_lbl succ_lbl in
                add_constraint ctx t (pred_srcs@succ_srcs) [succ_dst] new_lbl
              | [pred_src], [pred_dst], [succ_src], succ_dsts ->
                (* inclusion -> union *)
                assert((V.compare pred_dst v) = 0);
                assert((V.compare succ_src v) = 0);
                let new_lbl = ND.meet ctx pred_lbl succ_lbl in
                add_constraint ctx t [pred_src] succ_dsts new_lbl
              | [pred_src], pred_dsts, [succ_src], succ_dsts ->
                (* union -> inclusion
                 * union -> union
                 *   - can't take meet since these have an implicit disjunction, just take lhs
                *)
                assert((V.compare succ_src v) = 0);
                let pred_dsts = List.filter (fun pd -> (V.compare pd v) <> 0) pred_dsts in
                add_constraint ctx t [pred_src] (pred_dsts@succ_dsts) pred_lbl
              | [pred_src], pred_dsts, succ_src, [succ_dst] ->
                (* union -> intersection
                 *   - skip these
                *)
                t
              | pred_srcs, [pred_dst], [succ_src], succ_dsts ->
                (* intersection -> union
                 *   - skip these
                *)
                t
              | _ ->
                assert false
            ) t.g v t
        ) t.g v t in
      { t with
        g = G.remove_vtx t.g v}
    end


  let nd_top ctx based =
    ND.top ctx (BaseSym.to_int based)

  let nd_bottom ctx based =
    ND.bottom ctx (BaseSym.to_int based)

  let base_top ctx s =
    nd_top ctx (BaseSym.succ s.based)

  let base_bottom ctx s =
    nd_bottom ctx (BaseSym.succ s.based)

  let add_match_inclusion m src dst =
    let e = G.C.E.create [src,()] [dst,()] () in
    G.C.add_edge m e

  let add_match_union m src dst1 dst2 =
    let e = G.C.E.create [src,()] [dst1,(); dst2,()] () in
    G.C.add_edge m e
      
  (** [union_eq ctx s] finds patterns like:
      a <= b U c /\ b <= a /\ c <= a U b
      and simplifies it to the general case:
      a <= b u c /\ b <= a /\ c <= a
  *)
  let union_eq ctx s =
    let m = G.C.empty in
    let m = G.C.add_vtx m (G.MatchVariable 0) in
    let m = G.C.add_vtx m (G.MatchVariable 1) in
    let m = G.C.add_vtx m (G.MatchVariable 2) in
    let m = add_match_union m (G.MatchVariable 0) (G.MatchVariable 1) (G.MatchVariable 2) in
    let m = add_match_inclusion m (G.MatchVariable 1) (G.MatchVariable 0) in
    let m = add_match_union m (G.MatchVariable 2) (G.MatchVariable 0) (G.MatchVariable 1) in
    G.fold_match (fun vmap s ->
      let aid = G.IMap.find 0 vmap in
      (*let bid = Graph.IMap.find 1 vmap in*)
      let cid = G.IMap.find 2 vmap in
      add_constraint ctx s [cid] [aid] (base_top ctx s)
    ) s.g m s

  (** [rem_empty_edges ctx s] deletes all of the edges from the empty element *)
  let rem_empty_edges ctx s =
    let g = G.fold_succ (fun edge g ->
      G.remove_edge g edge
    ) s.g Empty s.g in
    { s with
      g = g;
    }

  (** [add_empty_edges ctx s] adds all of the edges from the empty element *)
  let add_empty_edges ctx s =
    let s = G.fold_vtxs (fun vtx s ->
      add_constraint ctx s [Empty] [vtx] (base_bottom ctx s)
    ) s.g s in
    s

  let add_eq_edges ctx s =
    let sccs = G.tarjans s.g in
    List.fold_left (fun s scc ->
      (* add all edges between elements in a scc *)
      Listx.fold_left_pairs (fun s el1 el2 ->
        let s = add_constraint ctx s [el1] [el2] (base_top ctx s) in
        let s = add_constraint ctx s [el2] [el1] (base_top ctx s) in
        s
      ) s scc
    ) s sccs

  let add_min_edges ctx s =
    let sccs = G.tarjans s.g in
    (*Format.printf "SCCS:@.";
    List.iter (fun scc ->
      Format.printf "  {%a}@." (Listx.fmt_str_sep ", " V.pp_print) scc
    ) sccs;*)
    let min_map = List.fold_left (fun min_map scc ->
      let scc = List.sort V.compare scc in
      let min_el = List.hd scc in
      List.fold_left (fun min_map el ->
        G.VMap.add el min_el min_map
      ) min_map scc
    ) G.VMap.empty sccs in
    G.fold_edges (fun edge s ->
      let srcs = G.E.get_src_vtxs edge in
      let dsts = G.E.get_dst_vtxs edge in
      let lbl = G.E.get_label edge in
      try
        let srcsa = List.map (fun src -> G.VMap.find src min_map) srcs in
        let dstsa = List.map (fun dst -> G.VMap.find dst min_map) dsts in
        (*Format.printf "min adding: %a: %a from %a@." fmt_edge (srcsa,dstsa) (fmt_base ctx) lbl fmt_edge (srcs,dsts);*)
        add_constraint ctx s srcsa dstsa lbl
      with Not_found ->
        assert false
    ) s.g s

  (** back propagage individual vertex constraints to edges and to self-edges *)
  let propagate_constraints ctx s =
    (*print_endline "Begin propagate_constraints";*)
    let module WL = WorkList.Make(V) in
    (* generate work list and basic map of vertex info *)
    let (vmap,wl) = G.fold_vtxs (fun vertex (vmap,wl) ->
      let vmap = match vertex with
        | Empty -> G.VMap.add vertex (base_bottom ctx s) vmap
        | _ ->  G.VMap.add vertex (base_top ctx s) vmap
      in
      (vmap, WL.add wl vertex)
    ) s.g (G.VMap.empty, WL.empty) in

    (* run work list *)
    let vmap = WL.run_fold (fun wl vertex vmap ->
      (*Format.printf "Visiting %a: " V.pp_print vertex;*)
      (* for each successor, propagate backwards *)
      let old_label = G.VMap.find vertex vmap in
      let label =  G.fold_succ (fun edge label ->
        let srcs = G.E.get_src_vtxs edge in
        let dsts = G.E.get_dst_vtxs edge in
        let lbl = G.E.get_label edge in
        (* if it is a union or normal edge *)
        match srcs, dsts with
          | [src], dsts ->
              assert((V.compare vertex src) = 0);
              (* get label by joining all elements of a union *)
              let ilbl = List.fold_left (fun ilbl dst ->
                ND.join ctx ilbl (G.VMap.find dst vmap)
              ) (base_bottom ctx s) dsts in
              (* take meet of the result with the edge label *)
              let upd_lbl = ND.meet ctx ilbl lbl in
              let upd_lbl = ND.meet ctx label upd_lbl in
              upd_lbl
          | _ -> label
      ) s.g vertex old_label in
      let modified = not (ND.le ctx old_label label) in
      (*Format.printf "modified: %B old_label: %a label: %a@." modified (fmt_base ctx) old_label (fmt_base ctx) label;*)

      let vmap = G.VMap.add vertex label vmap in

      (* if label was modified then add each predecessor vertex to the
         work list *)
      let wl = if modified then
          G.fold_pred (fun edge wl ->
            let vtxs = G.E.get_src_vtxs edge in
            match vtxs with
              | [v] -> WL.add wl v
              | _ -> wl
          ) s.g vertex wl
        else
          wl
      in
      (wl, vmap)
    ) wl vmap in

    (* strengthen edges with the results of the vmap *)
    let s = G.fold_edges (fun edge s ->
        let srcs = G.E.get_src_vtxs edge in
        let dsts = G.E.get_dst_vtxs edge in
        (*let lbl = G.E.get_label edge in*)
        let src_cnstr = List.fold_left (fun cnstr src ->
          ND.meet ctx cnstr (G.VMap.find src vmap)
        ) (base_top ctx s) srcs in
        let dst_cnstr = List.fold_left (fun cnstr dst ->
          ND.join ctx cnstr (G.VMap.find dst vmap)
        ) (base_bottom ctx s) dsts in
        let s = add_constraint ctx s srcs dsts src_cnstr in
        let s = add_constraint ctx s srcs dsts dst_cnstr in
        s
    ) s.g s in
    (* add self edges *)
    let s = G.VMap.fold (fun vtx lbl s ->
      (*Format.printf "vtx_cnstr: %a: %a@." V.pp_print vtx  (fmt_base ctx) lbl;*)
      add_constraint ctx s [vtx] [vtx] lbl
    ) vmap s in
    (*print_endline "Begin propagate_constraints";*)
    s

  let simplify ctx s =
    (*let s = add_universe_edges ctx s in
      let s = add_empty_edges ctx s in
      let s = add_eq_edges ctx s in*)
    let s = add_min_edges ctx s in
    (*let s = rem_universe_edges ctx s in*)
    let s = rem_empty_edges ctx s in
    let s = union_eq ctx s in
    let s = propagate_constraints ctx s in
    (*let s = merge_sccs ctx s in*)
    s

  let query_edge ctx s srcs dsts =
    let add_empty_lbl l = List.map (fun a -> a,()) l in
    let e = G.E.create (add_empty_lbl srcs) (add_empty_lbl dsts) (base_top ctx s) in
    try
      let e = G.get_edge s.g e in
      (s, Some (G.E.get_label e))
    with Not_found ->
      (s, None)

  let naive_get_constraint ctx s srcs dsts =
    match srcs,dsts with
      | [Empty], dsts ->
          (*Format.printf "  Case 1@.";*)
          (s, Some (base_bottom ctx s))
      | [src], dsts when (snd (query_edge ctx s [src] [Empty])) <> None ->
          (*Format.printf "  Case 2@.";*)
          (* one step to empty and empty is in everything *)
          (s, Some (base_bottom ctx s))
      (*| srcs, [Universe] ->
          Format.printf "  Case 3@.";
          (s, Some (base_top ctx s))*)
      | [src], [dst] when (V.compare src dst) = 0 ->
          (*Format.printf "  Case 4@.";*)
          let (s, res) = query_edge ctx s [src] [dst] in
          begin match res with
            | Some lbl ->
                (s, Some lbl)
            | None ->
                (s, Some (base_top ctx s))
          end
      | srcs, dsts ->
          (*Format.printf "  Case 5@.";*)
          query_edge ctx s srcs dsts

  (** [contained_in_explicit visited ctx s vtx] tests if a vertex is
      contained in a set of singleton vertexes.

      returns Some (set of vertexes), visited -- if contained
      else None, visited -- not contained

      visited holds already computed answers
  *)
  let rec contained_in_explicit visited ctx s vtx =
    (*Format.printf "contained: %a@." V.pp_print vtx;*)
    try
      let res = G.VMap.find vtx visited in
      (*Format.printf "already visited@.";*)
      (res, visited)
    with Not_found ->
      let visited = G.VMap.add vtx None visited in
      begin match vtx with
        | Empty ->
            let res = Some (G.VSet.empty, base_top ctx s) in
            (res, G.VMap.add vtx res visited)
        | Singleton _ ->
            let res = Some (G.VSet.singleton vtx, base_top ctx s) in
            (res, G.VMap.add vtx res visited)
        | Variable v ->
            if G.mem_vtx s.g (Variable v) then begin
              let visited = ref visited in
              let result = ref None in
              try
              (* get successors *)
                G.iter_succ (fun edge ->
                  let srcs = G.E.get_src_vtxs edge in
                  let dsts = G.E.get_dst_vtxs edge in
                  let lbl = G.E.get_label edge in
                  match srcs with
                    | [src] ->
                        assert((V.compare src vtx) = 0);
                        begin
                          try
                            let (res,l) = List.fold_left (fun (result,lbl) dst ->
                              let (res, vis) = contained_in_explicit !visited ctx s dst in
                              visited := vis;
                              match res with
                                | None -> raise Short_circuit
                                | Some (v,l) ->
                                    (G.VSet.union result v, ND.join ctx lbl l)
                            ) (G.VSet.empty,base_bottom ctx s) dsts in
                            result := Some (res,ND.meet ctx l lbl)
                          with Short_circuit ->
                          (* this successor edge didn't lead to a singleton *)
                            ()
                        end;
                        if !result <> None then
                          raise Short_circuit
                    | _ ->
                        ()
                ) s.g vtx;
              (None, !visited)
              with
                | Short_circuit ->
                  (* found a singleton set result! *)
                    assert(!result <> None);
                    (!result, G.VMap.add vtx !result !visited)
            end else
              (None, visited)
      end

  let contained_in_explicit ctx s vtx =
    let (res,_visited) = contained_in_explicit G.VMap.empty ctx s vtx in
    res

            

          
          
          
  (*module MC = struct

    type ops = {
    state : Z3.ast array -> Z3.ast;
    n_vtx: int;
    n_edge: int;
    vtx_map: int G.VMap.t;
    }

    let mk_ops ctx fp g =
  (* get vertex and edge counts *)
      let n_vtx = G.count_vtxs g in
      let n_edge = G.count_edges g in
      let (_,vtx_map) = G.fold_vtxs (fun vtx (count,map) ->
        (count+1,G.VMap.add vtx count map)
      ) g (0, G.VMap.empty) in
      (* create symbols *)
      let state_symb = Z3.mk_string_symbol ctx "state" in
      (* create functions *)
      let bs = Z3.mk_bool_sort ctx in
      let args = Array.make (n_vtx+n_edge) bs in
      let state_fun = Z3.mk_func_decl ctx state_symb args bs in
      (* register functions as relations *)
      Z3.fixedpoint_register_relation ctx fp state_fun;
      (* return ops object *)
      {
        state = (fun bits ->
          assert ((Array.length bits) = n_vtx + n_edge);
          Z3.mk_app ctx state_fun bits
        );
        n_vtx = n_vtx;
        n_edge = n_edge;
        vtx_map = vtx_map;
      }

    let mk_bound_app_of_str ctx name sort =
      let symb = Z3.mk_string_symbol ctx name in
      let cnst = Z3.mk_const ctx symb sort in
      (cnst,Z3.to_app ctx cnst)

    let mk_bound_app_of_num ctx num sort =
      let symb = Z3.mk_int_symbol ctx num in
      let cnst = Z3.mk_const ctx symb sort in
      (cnst,Z3.to_app ctx cnst)

    let mk_bound_apps ctx count sort =
      let (a,aa) = mk_bound_app_of_num ctx 0 sort in
      let a = Array.make count a in
      let aa = Array.make count aa in
      for i = 1 to count - 1 do
        let (t,ta) = mk_bound_app_of_num ctx i sort in
        a.(i) <- t;
        aa.(i) <- ta;
      done;
      (a,aa)

    let z3_add1 ctx ast =
      let one = Z3.mk_int ctx 1 (Z3.mk_int_sort ctx) in
      Z3.mk_add ctx [|ast; one|]

    let edge_rules ctx fp ops g is_union =
      let (a,aa) = mk_bound_apps ctx (ops.n_vtx + ops.n_edge) (Z3.mk_bool_sort ctx) in
      let edges = ref [] in
      let edge_num = ref ops.n_vtx in
      G.iter_edges (fun edge ->
        let edge_name = "e"^(string_of_int !edge_num) in
        edges := edge :: !edges;
        begin match edge with
          | G.Inclusion(src,dst) ->
              let src_vec = Array.copy a in
              let dst_vec = Array.copy a in
              (* set edge constraint *)
              src_vec.(!edge_num) <- Z3.mk_false ctx;
              dst_vec.(!edge_num) <- Z3.mk_true ctx;
              (* set vertex constraint *)
              let src_pos = IMap.find src ops.vtx_map in
              src_vec.(src_pos) <- Z3.mk_true ctx;
              dst_vec.(src_pos) <- Z3.mk_false ctx;
              dst_vec.(IMap.find dst ops.vtx_map) <- Z3.mk_true ctx;
              let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
              Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx edge_name);
          | G.Union(src,dst1,dst2) ->
              let src_vec = Array.copy a in
              let dst_vec = Array.copy a in
              (* set edge constraint *)
              src_vec.(!edge_num) <- Z3.mk_false ctx;
              dst_vec.(!edge_num) <- Z3.mk_true ctx;
              (* set vertex constraint *)
              let src_pos = IMap.find src ops.vtx_map in
              src_vec.(src_pos) <- Z3.mk_true ctx;
              dst_vec.(src_pos) <- Z3.mk_false ctx;
              dst_vec.(IMap.find dst1 ops.vtx_map) <- Z3.mk_true ctx;
              dst_vec.(IMap.find dst2 ops.vtx_map) <- Z3.mk_true ctx;
              let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
              if is_union then
                Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx edge_name);
          | G.Intersection(src1,src2,dst) ->
              let src_vec = Array.copy a in
              let dst_vec = Array.copy a in
              (* set edge constraint *)
              src_vec.(!edge_num) <- Z3.mk_false ctx;
              dst_vec.(!edge_num) <- Z3.mk_true ctx;
              (* set vertex constraint *)
              let src1_pos = IMap.find src1 ops.vtx_map in
              let src2_pos = IMap.find src2 ops.vtx_map in
              src_vec.(src1_pos) <- Z3.mk_true ctx;
              src_vec.(src2_pos) <- Z3.mk_true ctx;
              dst_vec.(src1_pos) <- Z3.mk_false ctx;
              dst_vec.(src2_pos) <- Z3.mk_false ctx;
              dst_vec.(IMap.find dst ops.vtx_map) <- Z3.mk_true ctx;
              let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
              if not is_union then
                Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx edge_name);
        end;
        incr edge_num
      ) g;
      let edges = List.rev !edges in
      Array.of_list edges

    let extension_rules ctx fp ops is_union =
      let (a,aa) = mk_bound_apps ctx (ops.n_vtx + ops.n_edge) (Z3.mk_bool_sort ctx) in
      (* can add extra terms on the rhs of a union or can remove extra terms the rhs of an intersection *)
      for i = 0 to ops.n_vtx - 1 do
        let edge_name = "add_rem_"^(string_of_int i) in
        let src_vec = Array.copy a in
        let dst_vec = Array.copy a in
        if is_union then begin
          dst_vec.(i) <- Z3.mk_true ctx
        end else begin
          dst_vec.(i) <- Z3.mk_false ctx
        end;
        let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
        Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx edge_name)
      done

    (* if an empty is present on the lhs, produce a new left-hand side in the following way
       lhs - {empty} + e, where e in 2^T.

       That is empty is less than anything, so add anything to what we have already.
    *)
    let empty_rules ctx fp ops is_union emp_id =
      let (a,aa) = mk_bound_apps ctx (ops.n_vtx + ops.n_edge) (Z3.mk_bool_sort ctx) in
      let src_vec = Array.copy a in
      let dst_vec = Array.copy a in
      let emp_index = IMap.find emp_id ops.vtx_map in
      if is_union then begin
        src_vec.(emp_index) <- (Z3.mk_true ctx);
        dst_vec.(emp_index) <- (Z3.mk_false ctx);
      end else begin
        src_vec.(emp_index) <- (Z3.mk_true ctx);
        Array.fill dst_vec 0 ops.n_vtx (Z3.mk_false ctx);
        dst_vec.(emp_index) <- (Z3.mk_true ctx);
      end;
      let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
      Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx "emp_rule")

    let universe_rules ctx fp ops is_union uvs_id =
      let (a,aa) = mk_bound_apps ctx (ops.n_vtx + ops.n_edge) (Z3.mk_bool_sort ctx) in
      let (cv,cva) = mk_bound_app_of_str ctx "count_var" (Z3.mk_int_sort ctx) in
      let aa = Array.append aa [|cva|] in
      let uvs_index = IMap.find uvs_id ops.vtx_map in
      let src_vec = Array.copy a in
      let dst_vec = Array.copy a in
      if is_union then begin
        src_vec.(uvs_index) <- (Z3.mk_true ctx);
        Array.fill dst_vec 0 ops.n_vtx (Z3.mk_false ctx);
        dst_vec.(uvs_index) <- (Z3.mk_true ctx);
      end else begin
        src_vec.(uvs_index) <- (Z3.mk_true ctx);
        dst_vec.(uvs_index) <- (Z3.mk_false ctx);
      end;
      let ast = Z3.mk_forall_const ctx 0 aa [||] (Z3.mk_implies ctx (ops.state src_vec) (ops.state dst_vec)) in
      Z3.fixedpoint_add_rule ctx fp ast (Z3.mk_string_symbol ctx "uvs_rule")

    let mk_query ctx fp ops src dst =
      let (a,aa) = mk_bound_apps ctx (ops.n_vtx + ops.n_edge) (Z3.mk_bool_sort ctx) in
      (* add the src rule *)
      let src_vec = Array.make (ops.n_vtx + ops.n_edge) (Z3.mk_false ctx) in
      List.iter (fun id ->
        let index = IMap.find id ops.vtx_map in
        src_vec.(index) <- Z3.mk_true ctx
      ) src;
      Z3.fixedpoint_add_rule ctx fp
        (ops.state src_vec)
        (Z3.mk_string_symbol ctx "src_rule");
      (* add the dst query *)
      (* TODO: add existential edge variables *)
      let dst_vec = Array.copy a in
      for i = 0 to ops.n_vtx - 1 do
        dst_vec.(i) <- Z3.mk_false ctx
      done;
      List.iter (fun id ->
        let index = IMap.find id ops.vtx_map in
        dst_vec.(index) <- Z3.mk_true ctx
      ) dst;
      let ast = Z3.mk_exists_const ctx 0 aa [||] (ops.state dst_vec) in
      (*print_endline (Z3.fixedpoint_to_string ctx fp [|ast|]);*)
      Z3.fixedpoint_query ctx fp ast

    let apply_rules ctx fp ops g emp_id uvs_id src dst is_union =
      let edges = edge_rules ctx fp ops g is_union in
      extension_rules ctx fp ops is_union;
      empty_rules ctx fp ops is_union emp_id;
      universe_rules ctx fp ops is_union uvs_id;
      (edges,mk_query ctx fp ops src dst)

    module IntMap = Mapx.Make(struct
      type t = int
      let compare = compare
    end)


    let handle_conjunct ctx ops dst ast =
      match Z3.term_refine ctx ast with
        | Z3.Term_numeral _ -> assert false
        | Z3.Term_quantifier _ -> assert false
        | Z3.Term_var _ -> assert false
        | Z3.Term_app (kind, decl, args) ->
            let decl_name = Z3.get_decl_name ctx decl in
            begin match Z3.symbol_refine ctx decl_name with
              | Z3.Symbol_string s when s = "state" ->
                  let bargs = Array.fold_right (fun el result ->
                    match Z3.term_refine ctx el with
                      | Z3.Term_app (Z3.OP_TRUE, _, _) ->
                          true::result
                      | Z3.Term_app (Z3.OP_FALSE, _, _) ->
                          false::result
                      | _ ->
                          print_endline (Z3.ast_to_string ctx ast);
                          assert false
                  ) args [] in
                  let bargs = Array.of_list bargs in
                  assert((Array.length bargs) = (ops.n_vtx + ops.n_edge));
                  let bvtxs = Array.sub bargs 0 ops.n_vtx in
                  let bedges = Array.sub bargs ops.n_vtx ops.n_edge in
                  assert((Array.append bvtxs bedges) = bargs);
                  Some (bvtxs,bedges)
              | _ -> None (* skip *)
            end

    let get_edges ctx fp ops dst =
      (*Format.printf "Looking for:@.";
      List.iter (fun el ->
        Format.printf "%a, " IDSym.fmt el
      ) dst;
      Format.printf "@.";*)
        
      (*let backmap = IMap.fold (fun symb int backmap ->
        IntMap.add int symb backmap
      ) ops.vtx_map IntMap.empty in*)
        
      let ast = Z3.fixedpoint_get_answer ctx fp in
      (* find conjunct that has the actual answer *)
      match Z3.term_refine ctx ast with
        | Z3.Term_numeral _ -> assert false
        | Z3.Term_quantifier _ -> assert false
        | Z3.Term_var _ -> assert false
        | Z3.Term_app (kind,_decl,args) ->
            assert(kind = Z3.OP_AND);
            let result = Array.fold_left (fun result element ->
              let relement = handle_conjunct ctx ops dst element in
              match relement with
                | Some e -> e::result
                | None -> result
            ) [] args in
            result

    module EdgeSet = Setx.Make(struct
      type t = G.edge_t
      let compare = G.compare_edges
    end)

    let compute_in_uvs uvs_id vtxb edge vtxa =
      let (srcs,dsts) = match edge with
        | G.Inclusion(src,dst) ->
            (ISet.singleton src, ISet.singleton dst)
        | G.Union(src,dst1,dst2) ->
            (ISet.singleton src, ISet.of_list [dst1;dst2])
        | G.Intersection(src1,src2,dst) ->
            (ISet.of_list [src1;src2], ISet.singleton dst)
      in
      let non_common_before = ISet.diff vtxb vtxa in
      (*let post_edge_set = ISet.union (ISet.diff vtxb src) dst in*)
      let pre_edge_set = ISet.union (ISet.diff vtxa dsts) srcs in
      let res = ISet.diff non_common_before pre_edge_set in
      (*Format.printf "res: %a, before: %a (%a -> %a) after: %a@." fmt_id_set res fmt_id_set vtxb fmt_id_set srcs fmt_id_set dsts fmt_id_set vtxa;*)
      assert((ISet.is_empty res) || (ISet.mem uvs_id vtxa));
      res

    let in_cut_and_in_uvs uvs_id results =
      List.fold_left (fun (in_cut,in_uvs) (vtxb,edge,vtxa) ->
        let in_uvs = ISet.union (compute_in_uvs uvs_id vtxb edge vtxa) in_uvs in
        let in_cut = EdgeSet.add edge in_cut in
        (in_cut,in_uvs)
      ) (EdgeSet.empty, ISet.empty) results

    let rec get_lbl_union nd_ctx based g visited uvs_id in_cut in_uvs vtx =
      (* if already visited, don't revisit *)
      try (visited,IMap.find vtx visited)
      with Not_found ->
        (* if not, check if it is a terminal *)
        begin
          try
            if ISet.mem vtx in_uvs then
              raise Short_circuit;
            G.iter_succs (fun edge _lbl ->
              if EdgeSet.mem edge in_cut then
                raise Short_circuit;
            ) g vtx;
            let result = nd_top nd_ctx based in
            (* it's a terminal, return top *)
            (IMap.add vtx result visited, result)
          with Short_circuit ->
            let visited = IMap.add vtx (nd_top nd_ctx based) visited in
            (* it's not a terminal, compute the appropriate result *)
            let (res,visited) = G.fold_succs (fun edge lbl (res, visited) ->
              (* get set of destinations *)
              let dsts = match edge with
                | G.Inclusion(src,dst) ->
                    [dst]
                | G.Union(src,dst1,dst2) ->
                    [dst1;dst2]
                | G.Intersection _ ->
                    []
              in
              (* compute result for edge (rese) *)
              let (rese,visited) = List.fold_left (fun (rese,visited) dst ->
                let (visited,r) = get_lbl_union nd_ctx based g visited uvs_id in_cut in_uvs dst in
                (ND.join nd_ctx rese r, visited)
              ) (nd_bottom nd_ctx based, visited) dsts in
              (* add result to other results *)
              let res = ND.join nd_ctx res (ND.meet nd_ctx lbl rese) in
              (res,visited)
            ) g vtx (nd_bottom nd_ctx based, visited) in
            let res, visited = if ISet.mem vtx in_uvs then begin
              let (visited,uvsres) = get_lbl_union nd_ctx based g visited uvs_id in_cut in_uvs uvs_id in
              let lbl =
                try
                  G.get_inclusion g vtx vtx
                with Not_found ->
                  nd_top nd_ctx based
              in
              (ND.join nd_ctx (ND.meet nd_ctx uvsres lbl) res, visited)
            end else
              (res,visited)
            in
            let visited = IMap.add vtx res visited in
            (visited, res)

        end

    let get_lbl_union nd_ctx based g uvs_id in_cut in_uvs vtx =
      let (_visited,res) = get_lbl_union nd_ctx based g IMap.empty uvs_id in_cut in_uvs vtx in
      res
               
        

    let get_lbl nd_ctx ctx g fp based ops emp_id uvs_id src dst edges is_union =
      let result_path = get_edges ctx fp ops dst in

      Listx.iteri (fun seq (bvtxs,bedges) ->
        Format.printf "%d :" seq;
        Array.iter (fun tf ->
          Format.printf "%d" (if tf then 1 else 0)
        ) bvtxs;
        Format.printf "|";
        Array.iter (fun tf ->
          Format.printf "%d" (if tf then 1 else 0)
        ) bedges;
        Format.printf "@.";
      ) result_path;

      let backmap = IMap.fold (fun id index m -> IntMap.add index id m) ops.vtx_map IntMap.empty in

      assert((List.length result_path) > 1);
      let (_count,_blocker, result) = List.fold_left (fun (count,blocker,result) (bvtxs,bedges) ->
        (* get set of vertexes enabled at this point *)
        let index = ref 0 in
        let vtxs = Array.fold_left (fun vtxs tf ->
          let vtxs = if tf then
              ISet.add (IntMap.find !index backmap) vtxs
            else
              vtxs
          in
          incr index;
          vtxs
        ) ISet.empty bvtxs in
        (* get the blocker *)
        let blocker = match blocker with
          | None -> Array.make (Array.length bedges) true
          | Some b -> b
        in
        (* get the edge and update the blocker *)
        let edge = ref None in
        Array.iteri (fun index tf ->
          if tf && (blocker.(index)) then begin
            assert(!edge = None);
            edge := Some (edges.(index));
            blocker.(index) <- false;
          end
        ) bedges;
        let edge = !edge in
        (count+1, Some blocker, (vtxs,edge)::result)
      ) (0, None, []) result_path in
      let result = List.rev result in
      assert((snd (List.hd result)) = None);
      let init = fst (List.hd result) in
      let (_prev,result) = List.fold_left (fun (prev,result) (vtxs,edge) ->
        match edge with
          | None ->
              (prev, result)
          | Some (edge,lbl) ->
              let result = (prev, edge, vtxs)::result in
              let prev = vtxs in
              (prev,result)
      ) (init,[]) (List.tl result) in
      let result = List.rev result in

      Listx.iteri (fun seq (vtxb, edge, vtxa) ->
        Format.printf "%d: %a ===> %a ===> %a@." seq fmt_id_set vtxb (fmt_edge_nnl ()) edge fmt_id_set vtxa
      ) result;

      Format.printf "--------------------@.";

      let (in_cut,in_uvs) = in_cut_and_in_uvs uvs_id result in
      if is_union then
        (* extract the non-empty element if there is one from the src *)
        let src = match src with
          | [a;b] when a = emp_id -> b
          | [b;a] when a = emp_id -> b
          | [a] -> a
          | _ ->
              Format.printf "%a@." (Listx.fmt_str_sep " " IDSym.fmt) src;
              assert false
        in
            
        get_lbl_union nd_ctx based g uvs_id in_cut in_uvs src
      else
        (* TODO: handle intersection edges appropriately *)
        nd_top nd_ctx based
        

        
      
      

    let run nd_ctx based g emp_id uvs_id src dst is_union =
      let ctx = Z3.mk_context ["MODEL","true"] in
      (* make a datalog engine *)
      let fp = Z3.mk_fixedpoint ctx in
      (* configure the datalog engine *)
      let fpparams = Z3.mk_params ctx in
      let engine = Z3.mk_string_symbol ctx ":engine" in
      let datalog = Z3.mk_string_symbol ctx "pdr" in
      Z3.params_set_symbol ctx fpparams engine datalog;
      let bitblast = Z3.mk_string_symbol ctx ":bit-blast" in
      Z3.params_set_bool ctx fpparams bitblast true;
      let slice = Z3.mk_string_symbol ctx ":slice" in
      Z3.params_set_bool ctx fpparams slice false;
      Z3.fixedpoint_set_params ctx fp fpparams;

      let ops = mk_ops ctx fp g in
      match apply_rules ctx fp ops g emp_id uvs_id src dst is_union with
        | (edges, Z3.L_TRUE) ->
            let res_lbl = get_lbl nd_ctx ctx g fp based ops emp_id uvs_id src dst edges is_union in
            (g, Some res_lbl)
        | (_,Z3.L_FALSE) ->
            (g,None)
        | (_,Z3.L_UNDEF) ->
            assert false


    let exists_edge_elems ctx based g emp_id uvs_id src dst =
      assert(G.mem_vertex g emp_id);
      assert(G.mem_vertex g uvs_id);
      let (is_union,src,dst) = if (List.length src) > (List.length dst) then
          (* intersection: add universal set *)
          (false, src, dst)
        else
          (* union: add empty set *)
          (true, src, dst)
      in
      let src = List.sort compare src in
      let src = Listx.uniq compare src in
      let dst = List.sort compare dst in
      let dst = Listx.uniq compare dst in
      (* remove duplicates *)
      (* TODO: support edge labels properly *)
      let (g,edges) = run ctx based g emp_id uvs_id src dst is_union in
      (*(g,edges)*)
      if edges = None then
        (g, None)
      else
        (g,Some (nd_top ctx based))

  end*)

  let get_constraint ctx s srcs dsts =
    naive_get_constraint ctx s srcs dsts

  let fold_constraints _ctx f s r =
    G.fold_edges (fun edge r ->
      let srcs = G.E.get_src_vtxs edge in
      let dsts = G.E.get_dst_vtxs edge in
      let lbl = G.E.get_label edge in
      f srcs dsts lbl r
    ) s.g r

  let fold_atoms _ctx f s r =
    G.fold_vtxs f s.g r

  let mem_atom _ctx s a =
    G.mem_vtx s.g a

  let replace_atom _ctx s a b =
    {s with g = G.replace_vtx s.g a b}

  let mapi_edges ctx f s =
    let s' = copy_atoms ctx s in
    let s' = fold_constraints ctx (fun srcs dsts lbl s' ->
      let lbl' = f srcs dsts lbl in
      add_constraint ctx s' srcs dsts lbl'
    ) s s' in
    s'

  let map_edges ctx f s =
    mapi_edges ctx (fun _srcs _dsts -> f) s

  let count_variables l =
    List.fold_left (fun count el ->
      match el with
        | Variable _ -> count + 1
        | _ -> count
    ) 0 l

  let delete_relations ctx s =
    let s' = copy_atoms ctx s in
    let s' = fold_constraints ctx (fun srcs dsts lbl s' ->
      let vcount = (count_variables srcs) + (count_variables dsts) in
      if vcount <= 1 then
        add_constraint ctx s' srcs dsts lbl
      else
        s'
    ) s s' in
    s'

  let delete_reductions ctx s =
    map_edges ctx (fun lbl -> base_top ctx s) s
    

end
