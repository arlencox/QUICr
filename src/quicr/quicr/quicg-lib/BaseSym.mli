type t
val zero : t
val succ : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val fmt : Format.formatter -> t -> unit
val compare : 'a -> 'a -> int
val of_int : int -> t
val to_int : t -> int
