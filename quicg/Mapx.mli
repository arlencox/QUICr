module type S = sig
  include Map.S

  val forall2 :
    (key -> 'a -> 'b -> bool) ->
    'a t ->
    'b t ->
    bool
      
  val exists2 :
    (key -> 'a -> 'b -> bool) ->
    'a t ->
    'b t ->
    bool
      
  val fmt :
    ?sep:(Format.formatter -> unit) ->
    ?kvsep:(Format.formatter -> unit) ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val foldi :
    (int -> key -> 'a -> 'b -> 'b) ->
    'a t ->
    'b ->
    'b

  val fold2 :
    (key -> 'a option -> 'b option -> 'c -> 'c) ->
    'a t ->
    'b t ->
    'c -> 'c

  val of_assn_list :
    (key * 'a) list ->
    'a t

  val to_assn_list :
    'a t -> (key * 'a) list
end

module Make : functor (Ord : Map.OrderedType) -> S with type key = Ord.t
