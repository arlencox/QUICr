module type UnionFind = sig
  (** [ctx] is context for this union find structure *)
  type ctx

  (** [t] is a set<r> data structure *)
  type t

  (** [elt] is an element the sets *)
  type elt

  module ESet : Set.S with type elt = elt
  module EMap : Map.S with type key = elt

  val init : unit -> ctx

  (** [empty] returns the empty set of sets *)
  val empty : ctx -> t

  (** [is_empty] returns true if this is an empty mapping (either empty or only
      singleton sets *)
  val is_empty : t -> bool

  (** [context t] retrieves the context used in the construction of this [t] *)
  val context : t -> ctx

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  (* [remove e t] removes an element from the sets.  It returns an updated sets
     and a value that determines what happened to the representative:
     [NoRepresentative]    - There is no new representative
     [SameRepresentative]  - The representative did not change

     [NewRepresentative r] - The element removed was a representative and the
                             new representative for this set is [r] *)
  val remove : elt -> t -> t * rem

  (** [mem e t] returns true if [e] is in one of the sets explicitly described
      by [t] *)
  val mem : elt -> t -> bool

  (** [rep e t] returns the representative for the element [e] *)
  val rep : elt -> t -> elt

  (** [reps t] returns the set of all representatives *)
  val reps : t -> elt list

  (** [elements e t] returns the set of elements in the set with [e] within [t]
  *)
  val elements : elt -> t -> ESet.t

  (** [union e1 e2 t] unions the two sets [e1] and [e2] within [t], adding [e1] and [e2] if necessary *)
  val union : elt -> elt -> t -> t

  (** [merge t1 t2] produces [(t,r1,r2)].  [t] is the "meet" of [t1] and [t2].
      Precisely, the following is true of [t]:
        Forall s1, s2.  (rep s1 t1) = (rep s2 t1) => (rep s1 t) = (rep s2 t)
                   and  (rep s1 t2) = (rep s2 t2) => (rep s1 t) = (rep s2 t)

      [r1] and [r2] are (ideally minimal) tranformation mappings that can be
      applied to [t1] and [t2] respectively to make them represent the same
      set of sets as [t].  Precisely, the following is true:
        Exist bijection map1.  Forall s.  ((rep s t1), (rep s t)) in map1.
        and Exist bijection map2.  Forall s.  ((rep s t2), (rep s t)) in map2.
  *)
  val merge : t -> t -> t

  (** [split t1 t2] produces [(t,r1,r2)]. [t] is the "join" of [t1] and [t2].
      Precisely, the following is true of [t]:
        Forall s1, s2.  (rep s1 t1) = (rep s2 t1) => (rep s1 t) = (rep s2 t)
                    or  (rep s1 t2) = (rep s2 t2) => (rep s1 t) = (rep s2 t)

      [r1] and [r2] are (ideally minimal) tranformation mappings that can be
      applied to [t] to make it represent the same set of sets as [t1] and [t2]
      respectively.  Precisely, the following is true:
        Exist bijection map1.  Forall s.  ((rep 
  *)
  val split : t -> t -> t

  (** [diff t1 t2] takes two union-find structures [t1] and [t2], where [t1] is
      a refinement of [t2] (two elements MAY be in the same set in [t1] if they
      are in the same set in [t2].  It returns a list of pairs of elements that
      must be merged to transform [t1] into [t2]. *)
  val diff : t -> t -> (elt * elt) list

  (** [le t1 t2] queries if [t2] is a refinement of [t1].  That is if two
      elements are in the same set in [t2], they must be in the same set in
      [t1] *) val le : t -> t -> bool
  val fold : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** [rename map t] renames elements in [t] according to the mapping [map]. *)
  val rename : elt Rename.t -> t -> t

  (** [pairs t ] produces a list of all pairs needed to construct the
      union-find structure *)
  val pairs : t -> (elt * elt) list
end
