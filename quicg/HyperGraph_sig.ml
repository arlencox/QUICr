module type Comparable_Printable =
  sig
    type t
    val compare : t -> t -> int
    val pp_print : Format.formatter -> t -> unit
  end

module type HyperGraph =
sig
  type vtx_t
  type edge_t
  type src_lbl_t
  type dst_lbl_t
  type lbl_t
  module VMap: Mapx.S with type key = vtx_t
  module VSet: Setx.S with type elt = vtx_t
  module EMap: Mapx.S with type key = edge_t
  module ESet: Setx.S with type elt = edge_t
  module V :
  sig
    type t = vtx_t
    val compare : t -> t -> int
    val pp_print : Format.formatter -> t -> unit
  end
  module E :
  sig
    type t = edge_t
    val compare : t -> t -> int
    val pp_print :
      ?print_src_vtx:(Format.formatter -> vtx_t * src_lbl_t -> unit) ->
      ?print_dst_vtx:(Format.formatter -> vtx_t * dst_lbl_t -> unit) ->
      ?src_vtx_sep:(Format.formatter -> unit -> unit) ->
      ?dst_vtx_sep:(Format.formatter -> unit -> unit) ->
      ?vtx_wrapl:(Format.formatter -> unit -> unit) ->
      ?vtx_wrapr:(Format.formatter -> unit -> unit) ->
      ?edge_draw:(Format.formatter -> (Format.formatter -> unit -> unit) * (Format.formatter -> unit -> unit) * lbl_t -> unit) ->
      unit -> Format.formatter -> t -> unit
    val get_src_vtxs : t -> vtx_t list
    val get_dst_vtxs : t -> vtx_t list
    val get_srcs : t -> (vtx_t * src_lbl_t) list
    val get_dsts : t -> (vtx_t * dst_lbl_t) list
    val get_label : t -> lbl_t
    val iter_srcs : (vtx_t -> src_lbl_t -> unit) -> t -> unit
    val iter_dsts : (vtx_t -> dst_lbl_t -> unit) -> t -> unit
    val fold_srcs :
      (vtx_t -> src_lbl_t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_dsts :
      (vtx_t -> dst_lbl_t -> 'a -> 'a) -> t -> 'a -> 'a
    val choose_src : t -> vtx_t
    val choose_dst : t -> vtx_t
    val create :
      (vtx_t * src_lbl_t) list ->
      (vtx_t * dst_lbl_t) list -> lbl_t -> t
    val get_vtxs : t -> VSet.t
    val replace_vtx : t -> vtx_t -> vtx_t -> t
  end
  type t

  val add_edge : t -> edge_t -> t
  val mem_edge : t -> edge_t -> bool
  val get_edge : t -> edge_t -> edge_t
  val remove_edge : t -> edge_t -> t
  val iter_succ : (edge_t -> unit) -> t -> vtx_t -> unit
  val fold_succ :
    (edge_t -> 'a -> 'a) -> t -> vtx_t -> 'a -> 'a
  val iter_pred : (edge_t -> unit) -> t -> vtx_t -> unit
  val fold_pred :
    (edge_t -> 'a -> 'a) -> t -> vtx_t -> 'a -> 'a
  val iter_edges : (edge_t -> unit) -> t -> unit
  val fold_edges : (edge_t -> 'a -> 'a) -> t -> 'a -> 'a
  val add_vtx : t -> vtx_t -> t
  val mem_vtx : t -> vtx_t -> bool
  val iter_vtxs : (vtx_t -> unit) -> t -> unit
  val fold_vtxs : (vtx_t -> 'a -> 'a) -> t -> 'a -> 'a
  val remove_vtx : t -> vtx_t -> t
  val replace_vtx : t -> vtx_t -> vtx_t -> t
  val empty : t
  val is_empty : t -> bool
  val max_vtx : t -> vtx_t
  val count_vtxs : t -> int
  val count_edges : t -> int
  val pp_debug :
    ?fmt_vtx:(Format.formatter -> vtx_t -> unit) ->
    ?fmt_src_lbl:(Format.formatter -> src_lbl_t -> unit) ->
    ?fmt_dst_lbl:(Format.formatter -> dst_lbl_t -> unit) ->
    ?fmt_edge_lbl:(Format.formatter -> lbl_t -> unit) ->
    unit -> Format.formatter -> t -> unit
  val tarjans : t -> vtx_t list list
  val sccs : t -> vtx_t list list
end

module type HyperGraphMatch =
sig
  include HyperGraph

  module IMap: Mapx.S with type key = int
  module ISet: Setx.S with type elt = int

  type match_vtx_t =
    | MatchVertex of vtx_t
    | MatchVariable of int

  module C: HyperGraph
    with type vtx_t = match_vtx_t
    and type src_lbl_t = unit
    and type dst_lbl_t = unit
    and type lbl_t = unit

  val iter_match :
    (vtx_t IMap.t -> unit) -> t -> C.t -> unit

  val fold_match :
    (vtx_t IMap.t -> 'a -> 'a) -> t -> C.t -> 'a -> 'a
end


