module type S = sig
  include Set.S
  
  val of_list: elt list -> t

  val map: (elt -> elt) -> t -> t

  val fmt:
    ?sep:(Format.formatter -> unit) ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter ->
    t ->
    unit

  val iter_subsets: (t -> unit) -> t -> unit
  val fold_subsets: (t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make : functor(Ord: Set.OrderedType) -> S with type elt = Ord.t
