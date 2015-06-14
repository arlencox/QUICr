module Make :
  functor (VT : HyperGraph_sig.Comparable_Printable) ->
    functor (SL : HyperGraph_sig.Comparable_Printable) ->
      functor (DL : HyperGraph_sig.Comparable_Printable) ->
        functor (L : HyperGraph_sig.Comparable_Printable) ->
          (HyperGraph_sig.HyperGraph
           with type vtx_t = VT.t
           and type src_lbl_t = SL.t
           and type dst_lbl_t = DL.t
           and type lbl_t = L.t)
