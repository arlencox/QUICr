type 'sym t
type 'sym c

val singleton : 'sym -> 'sym -> 'sym t
val of_assoc_list : ('sym * 'sym) list -> 'sym t
val of_iter_mem_get :
  (('sym -> 'sym -> unit) -> unit) -> ('sym -> bool) -> ('sym -> 'sym) -> 'sym t

val fold : ('sym -> 'sym -> 'b -> 'b) -> 'sym t -> 'b -> 'b
val iter : ('sym -> 'sym -> unit) -> 'sym t -> unit
val mem : 'sym t -> 'sym -> bool
val get : 'sym t -> 'sym -> 'sym

val to_assoc_list : 'sym t -> ('sym * 'sym) list

val empty : 'sym c
val prepend : 'sym t -> 'sym c -> 'sym c
val append : 'sym c -> 'sym t -> 'sym c
val compose : 'sym c -> 'sym c -> 'sym c
val of_composition : 'sym c -> 'sym t
