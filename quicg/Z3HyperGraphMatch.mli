module Make:
  functor (HG : HyperGraph_sig.HyperGraph) ->
  (HyperGraph_sig.HyperGraphMatch
   with type vtx_t = HG.vtx_t
   and type edge_t = HG.edge_t
   and type lbl_t = HG.lbl_t
   and type src_lbl_t = HG.src_lbl_t
   and type dst_lbl_t = HG.dst_lbl_t)
