
(** [map_filter f l] performs a map operation where if the result is none, it
    is not included in the resulting list.
    @param f function to be applied to each element
    @param l list from which to get the elements
    *)
val map_filter : ('a -> 'b option) -> 'a list -> 'b list

(** [rev_map_filter f l] like [map_filter f l] except that resulting list is
    reversed.  This is tail recursive
    @param f function to be applied to each element
    @param l list from which to get the elements
    *)
val rev_map_filter : ('a -> 'b option) -> 'a list -> 'b list

val fmt : (Format.formatter -> unit) -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val fmt_str_sep : string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val reduce_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val fold_lefti : (int -> 'a -> 'b ->'a) -> 'a -> 'b list -> 'a

val find_first : ('a -> 'a option) -> 'a list -> 'a option

val fold_seq : (int -> 'a -> 'a) -> int -> int -> 'a -> 'a

val uniq : ('a -> 'a -> int) -> 'a list -> 'a list

val compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int

val fold_left_pairs : ('b -> 'a -> 'a -> 'b) -> 'b -> 'a list -> 'b
