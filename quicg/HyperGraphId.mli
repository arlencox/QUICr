type vtx_id
type edge_id

type ('vl, 'el) t

val empty : ('vl, 'el) t

val add_edge :
  ('vl, 'el) t -> vtx_id list -> vtx_id list -> 'el -> ('vl, 'el) t * edge_id

val edge_id :
  ('vl, 'el) t -> vtx_id list -> vtx_id list -> edge_id

val mem_edge : ('vl, 'el) t -> vtx_id list -> vtx_id list -> bool
val edge_label : ('vl, 'el) t -> edge_id -> 'el
val remove_edge : ('vl, 'el) t -> vtx_id list -> vtx_id list -> ('vl, 'el) t
val add_vertex : ('vl, 'el) t -> 'vl -> ('vl, 'el) t * vtx_id
val vertex_label : ('vl, 'el) t -> vtx_id -> 'vl
val mem_vertex : ('vl, 'el) t -> vtx_id -> bool
val remove_vertex : ('vl, 'el) t -> vtx_id -> ('vl, 'el) t
val edge_count : ('vl, 'el) t -> int
val vertex_count : ('vl, 'el) t -> int
val iter_succ : (vtx_id list -> vtx_id list -> edge_id -> unit) ->
  ('vl, 'el) t -> vtx_id -> unit
val fold_succ : (vtx_id list -> vtx_id list -> edge_id -> 'a -> 'a) ->
  ('vl, 'el) t -> vtx_id -> 'a -> 'a
val iter_pred :
  (vtx_id list -> vtx_id list -> edge_id -> unit) ->
  ('vl, 'el) t -> vtx_id -> unit
val iter_edge :
  (vtx_id list -> vtx_id list -> edge_id -> unit) -> ('vl, 'el) t -> unit
val iter_vertex : (vtx_id -> unit) -> ('vl, 'el) t -> unit
val pp_dot :
  (Format.formatter -> 'vl -> unit) ->
  (Format.formatter -> 'el -> unit) -> Format.formatter -> ('vl, 'el) t -> unit
