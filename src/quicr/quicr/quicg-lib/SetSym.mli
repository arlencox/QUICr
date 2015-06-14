type t
val zero : t
val succ : t -> t
val pred : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val fmt : Format.formatter -> t -> unit
val compare : 'a -> 'a -> int
val fold_seq : (t -> 'a -> 'a) -> t -> t -> 'a -> 'a
val of_int : int -> t
val to_int : t -> int
