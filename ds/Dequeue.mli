type 'a t

val empty : 'a t
val singleton : 'a -> 'a t
val of_list : 'a list -> 'a t

val is_empty : 'a t -> bool

val push_back : 'a -> 'a t -> 'a t
val push_front : 'a -> 'a t -> 'a t

val pop_front : 'a t -> 'a t * 'a
val pop_back : 'a t -> 'a t * 'a

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit
val riter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val rfold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
