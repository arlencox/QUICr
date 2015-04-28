module type Interface = sig
  (** [t] is a set<r> data structure *)
  type t

  (** [elt] is an element the sets *)
  type elt

  (** [empty] returns the empty set of sets *)
  val empty : t

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  (* [remove e t] removes an element from the sets.  It returns an updated sets
     and a value that determines what happened to the representative:
     [NoRepresentative] - There is no new representative
     [SameRepresentative] - The representative did not change
     [NewRepresentative r] - The element removed was a representative and the new representative for this set is [r] *)
  val remove : elt -> t -> t * rem

  (** [mem e t] returns true if [e] is in one of the sets described by [t] *)
  val mem : elt -> t -> bool

  (** [rep e t] returns the representative for the element [e] *)
  val rep : elt -> t -> elt

  (** [reps t] returns the set of all representatives *)
  val reps : t -> elt list

  (** [elements e t] returns the set of elements in the set with [e] within [t] *)
  val elements : elt -> t -> elt list

  (** [union e1 e2 t] unions the two sets [e1] and [e2] within [t], adding [e1] and [e2] if necessary *)
  val union : elt -> elt -> t -> t

  (** [merge t1 t2] produces a new set of sets where each element that is in
      the same set as all elements it shares in [t1] and [t2].  Also returns a
      list of mappings from elements to new representatives.  Elements not
      remapped have the same representation as in both [t1] and [t2]. *)
  val merge : t -> t -> t * (elt * elt) list * (elt * elt) list

  val fold : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a

  val rename : (elt * elt) list -> t -> t * (elt * elt) list
end
