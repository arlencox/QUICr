module Make(HG: HyperGraph_sig.HyperGraph) :
  (HyperGraph_sig.HyperGraphMatch
   with type vtx_t = HG.vtx_t
   and type edge_t = HG.edge_t
   and type lbl_t = HG.lbl_t
   and type src_lbl_t = HG.src_lbl_t
   and type dst_lbl_t = HG.dst_lbl_t) =
struct
  include HG

  module IMap = Mapx.Make(struct
    type t = int
    let compare = compare
  end)
  module ISet = Setx.Make(struct
    type t = int
    let compare = compare
  end)

  type match_vtx_t =
    | MatchVertex of HG.vtx_t
    | MatchVariable of int

  module Unit =
  struct
    type t = unit
    let compare _a _b = 0
    let pp_print ff v = ()
  end

  module VC = struct
    type t = match_vtx_t
    let compare a b =
      match a,b with
        | MatchVariable a, MatchVariable b ->
            compare a b
        | MatchVertex a, MatchVertex b ->
            HG.V.compare a b
        | MatchVertex _, _ -> -1
        | MatchVariable _, _ -> 1

    let pp_print ff v =
      match v with
        | MatchVertex v -> HG.V.pp_print ff v
        | MatchVariable i -> Format.fprintf ff "'%d" i
  end

  module C = HyperGraph.Make(VC)(Unit)(Unit)(Unit)

  module SMT =
  struct

    module IIMap = Map.Make(struct
      type t = int * int
      let compare (a1,a2) (b1,b2) =
        let res = compare a1 b1 in
        if res = 0 then
          compare a2 b2
        else
          res
    end)

    type z3_graph = {
      edge_funs: (Z3.Expr.expr list -> Z3.Expr.expr list -> Z3.Expr.expr) IIMap.t;
      bound_function: Z3.Expr.expr -> unit;
      z3_to_node: vtx_t IMap.t;
      node_to_z3: int VMap.t;
      sort: Z3.Sort.sort;
    }

    let assert_cnstr ctx s ast =
      (*let str = Z3.Expr.expr_to_string ctx ast in
        Format.printf "%s@." str;*)
      Z3.Solver.add s ast

    let print_cnstr ctx ast =
      let str = Z3.Expr.to_string ast in
      Format.printf "%s@." str

    let graph2z3 ctx s t =
      let max_index = count_vtxs t in
      let fin_sort = Z3.Arithmetic.Integer.mk_sort ctx in
      let empty_graph = {
        edge_funs = IIMap.empty;
        bound_function = (fun a ->
            assert_cnstr ctx s [Z3.Arithmetic.mk_lt ctx a (Z3.Expr.mk_numeral_int ctx max_index fin_sort)];
            assert_cnstr ctx s [Z3.Arithmetic.mk_ge ctx a (Z3.Expr.mk_numeral_int ctx 0 fin_sort)]);
        z3_to_node = IMap.empty;
        node_to_z3 = VMap.empty;
        sort = fin_sort;
      } in
    (* add mappings *)
      let (_i,g) = fold_vtxs (fun n (i,g) ->
        (i+1,{g with
          z3_to_node = IMap.add i n g.z3_to_node;
          node_to_z3 = VMap.add n i g.node_to_z3;
        })
      ) t (0,empty_graph) in
    (* generate functions *)
      let g = fold_edges (fun edge g ->
        let srcs = E.get_src_vtxs edge in
        let dsts = E.get_dst_vtxs edge in
        let srcsc = List.length srcs in
        let dstsc = List.length dsts in
        let func =
          try IIMap.find (srcsc,dstsc) g.edge_funs
          with Not_found ->
            (fun srcs dsts -> Z3.Boolean.mk_false ctx)
        in
        let z3srcs = List.map (fun src ->
          let src = VMap.find src g.node_to_z3 in
          Z3.Expr.mk_numeral_int ctx src fin_sort
        ) srcs in
        let z3dsts = List.map (fun dst ->
          let dst = VMap.find dst g.node_to_z3 in
          Z3.Expr.mk_numeral_int ctx dst fin_sort
        ) dsts in
        let func isrcs idsts =
          let src_eqs = List.map2 (fun isrc z3src -> Z3.Boolean.mk_eq ctx isrc z3src) isrcs z3srcs in
          let dst_eqs = List.map2 (fun idst z3dst -> Z3.Boolean.mk_eq ctx idst z3dst) idsts z3dsts in
          let src_eqs = Z3.Boolean.mk_and ctx src_eqs in
          let dst_eqs = Z3.Boolean.mk_and ctx dst_eqs in
          let eq = Z3.Boolean.mk_and ctx [ src_eqs; dst_eqs ] in
          Z3.Boolean.mk_or ctx [ func isrcs idsts; eq ]
        in
        { g with
          edge_funs = IIMap.add (srcsc,dstsc) func g.edge_funs;
        }
      ) t g in
      g


    exception Short_circuit
        
    (* iterate over t finding all matches of (v,e) within, calling f passing it an
     * array of vertexes correspinding to the vertex variables in s *)
    let iter_match f t m =
      let ctx = Z3.mk_context ["MODEL","true"] in
      let s = Z3.Solver.mk_simple_solver ctx in
      (* convert graph to z3 format *)
      let g = graph2z3 ctx s t in
      (* enumerate variables *)
      let vars = C.fold_vtxs (fun vtx vars ->
        match vtx with
          | MatchVertex _ -> vars
          | MatchVariable v -> ISet.add v vars
      ) m ISet.empty in
      (* declare and bound variables *)
      let (vars_ast,vmap) = ISet.fold (fun var (vars_ast,vmap) ->
        let var_symbol = Z3.Symbol.mk_int ctx var in
        let decl = Z3.FuncDecl.mk_const_decl ctx var_symbol g.sort in
        let var_ast = Z3.FuncDecl.apply decl [] in
        g.bound_function var_ast;
        (var_ast::vars_ast,IMap.add var (var_symbol,var_ast) vmap)
      ) vars ([],IMap.empty) in
      assert_cnstr ctx s [Z3.Boolean.mk_distinct ctx vars_ast];
      (* convert match vertex to a z3 ast *)
      let match_vertex2z3 = function
        | MatchVariable v ->
            snd (IMap.find v vmap)
        | MatchVertex v ->
            Z3.Expr.mk_numeral_int ctx (VMap.find v g.node_to_z3) g.sort
      in

      (* assert edges *)
      C.iter_edges (fun edge ->
        let srcs = C.E.get_src_vtxs edge in
        let dsts = C.E.get_dst_vtxs edge in
        let srcsc = List.length srcs in
        let dstsc = List.length dsts in
        let func =
          try IIMap.find (srcsc,dstsc) g.edge_funs
          with Not_found ->
            (fun srcs dsts -> Z3.Boolean.mk_false ctx)
        in
        let z3srcs = List.map match_vertex2z3 srcs in
        let z3dsts = List.map match_vertex2z3 dsts in
        let asrt = func z3srcs z3dsts in
        assert_cnstr ctx s [asrt]
      ) m;
      let rec iter_model () =
        match Z3.Solver.check s [] with
          | Z3.Solver.SATISFIABLE ->
            let model = match Z3.Solver.get_model s with
              | Some m -> m | None -> assert false
            in
              let result = IMap.map (fun (_symb,ast) ->
                let east = Z3.Model.eval model ast true in
                let east = match east with
                  | Some east -> east
                  | None -> assert false
                in
                assert(Z3.Expr.is_numeral east);
                let i = Z3.Arithmetic.Integer.get_int east in
                IMap.find i g.z3_to_node
              ) vmap
              in
              let blocking_map = ref (IMap.map (fun i -> ISet.singleton (VMap.find i g.node_to_z3)) result) in
              (* TODO: remove some redundancy in the iteration *)
              let blocking_clause = ref (Z3.Boolean.mk_false ctx) in
              IMap.iter (fun variable values ->
                let ast_var = snd(IMap.find variable vmap) in
                let z3_values = List.rev_map (fun i -> Z3.Expr.mk_numeral_int ctx i g.sort) (ISet.elements values) in
                let cnstr = Z3.Boolean.mk_distinct ctx (ast_var::z3_values) in
                blocking_clause := Z3.Boolean.mk_or ctx [ cnstr; !blocking_clause ]
              ) !blocking_map;
              assert_cnstr ctx s [!blocking_clause];
              f result;
              (* TODO: fix memory leak: delete model*)
              iter_model ()
          | Z3.Solver.UNSATISFIABLE ->
              ()
          | Z3.Solver.UNKNOWN ->
              assert false
      in
      iter_model ();
      ()
  end

  let iter_match f g m =
    SMT.iter_match f g m

  let fold_match f t m r =
    let r = ref r in
    iter_match (fun result ->
      r := f result !r
    ) t m;
    !r
    
end
