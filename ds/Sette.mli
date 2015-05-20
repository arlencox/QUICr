(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Sets over ordered types (extension of standard library module and polymorphic variant) *)

(** This module implements the set data structure, given a total ordering
    function over the set elements. All operations over sets are purely
    applicative (no side-effects).  The implementation uses balanced binary
    trees, and is therefore reasonably efficient: insertion and membership
    take time logarithmic in the size of the set, for instance.

    Modified by B. Jeannet to get a generic type and a few additions
    (like conversions form and to maps and pretty-printing).
*)

type 'a set = 
  | Empty
  | Node of ('a set) * 'a * ('a set) * int
  (** Meant to be internal, but exporting needed for Mappe.maptoset. *)

type 'a t = 'a set
(** The type of sets over elements of type 'a. *)

val empty: 'a t
(** The empty set. *)
val is_empty: 'a t -> bool
(** Test whether a set is empty or not. *)
val mem: 'a -> 'a t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)
val add: 'a -> 'a t -> 'a t
(** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
val singleton: 'a -> 'a t
(** [singleton x] returns the one-element set containing only [x]. *)
val remove: 'a -> 'a t -> 'a t
(** [remove x s] returns a set containing all elements of [s], except
    [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union: 'a t -> 'a t -> 'a t
val inter: 'a t -> 'a t -> 'a t
val diff: 'a t -> 'a t -> 'a t
(** Union, intersection and set difference. *)

val compare: 'a t -> 'a t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)
val equal: 'a t -> 'a t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
    equal, that is, contain equal elements. *)
val subset: 'a t -> 'a t -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of
    the set [s2]. *)
val iter: ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
    The order in which the elements of [s] are presented to [f]
    is unspecified. *)
val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
    where [x1 ... xN] are the elements of [s].
    The order in which elements of [s] are presented to [f] is
    unspecified.
    @param f function
    @param s set
    @param a accumulator
    @return the computed accumulator
    @raise Not_found if no fount
*)
val for_all: ('a -> bool) -> 'a t -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. *)
val exists: ('a -> bool) -> 'a t -> bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. *)
val filter: ('a -> bool) -> 'a t -> 'a t
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)
val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p s] returns a pair of sets [(s1, s2)], where [s1] is the
    set of all the elements of [s] that satisfy the predicate [p], and [s2]
    is the set of all the elements of [s] that do not satisfy [p]. *)
val cardinal: 'a t -> int
(** Return the number of elements of a set. *)
val elements: 'a t -> 'a list
(** Return the list of all elements of the given set.  The returned list
    is sorted in increasing order with respect to the ordering
    [Pervasives.compare]. *)
val min_elt: 'a t -> 'a 
(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering), or raise [Not_found] if the set is empty. *)
val max_elt: 'a t -> 'a
(** Same as [min_elt], but returns the largest element of the given
    set. *)
val choose: 'a t -> 'a
(** Return one element of the given set, or raise [Not_found] if the set
    is empty. Which element is chosen is unspecified, but equal elements
    will be chosen for equal sets. *)
val print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** *)

(** Output signature of the functor {!Sette.Make}. *)
module type S = 
sig
  type elt
  (** The type of the set elements. *)

  type t
  (** The type of sets. *)

  val repr : t -> elt set
  val obj : elt set -> t

  module Ord : (Set.OrderedType with type t=elt)
  (** The ordering module used for this set module. *)

  val empty: t
  (** The empty set. *)

  val is_empty: t -> bool
  (** Test whether a set is empty or not. *)

  val mem: elt -> t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)

  val add: elt -> t -> t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val singleton: elt -> t
  (** [singleton x] returns the one-element set containing only [x]. *)

  val remove: elt -> t -> t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)

  val union: t -> t -> t
  (** Set union. *)

  val inter: t -> t -> t
  (** Set intersection. *)

  (** Set difference. *)
  val diff: t -> t -> t

  val compare: t -> t -> int
  (** Total ordering between sets. Can be used as the ordering function
      for doing sets of sets. *)

  val equal: t -> t -> bool
  (** [equal s1 s2] tests whether the sets [s1] and [s2] are
      equal, that is, contain equal elements. *)

  val subset: t -> t -> bool
  (** [subset s1 s2] tests whether the set [s1] is a subset of
      the set [s2]. *)

  val iter: (elt -> unit) -> t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s].
      The order in which the elements of [s] are presented to [f]
      is unspecified. *)

  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s].
      The order in which elements of [s] are presented to [f] is
      unspecified.
  *)

  val for_all: (elt -> bool) -> t -> bool
  (** [for_all p s] checks if all elements of the set
      satisfy the predicate [p]. *)

  val exists: (elt -> bool) -> t -> bool
  (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)

  val filter: (elt -> bool) -> t -> t
  (** [filter p s] returns the set of all elements in [s]
      that satisfy predicate [p]. *)

  val partition: (elt -> bool) -> t -> t * t
  (** [partition p s] returns a pair of sets [(s1, s2)], where
      [s1] is the set of all the elements of [s] that satisfy the
      predicate [p], and [s2] is the set of all the elements of
      [s] that do not satisfy [p]. *)

  val cardinal: t -> int
  (** Return the number of elements of a set. *)

  val elements: t -> elt list
  (** Return the list of all elements of the given set.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Sette.Make}. *)

  val min_elt: t -> elt
  (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the set is empty. *)

  val max_elt: t -> elt
  (** Same as {!Sette.S.min_elt}, but returns the largest element of the
      given set. *)

  val choose: t -> elt
  (** Return one element of the given set, or raise [Not_found] if
      the set is empty. Which element is chosen is unspecified,
      but equal elements will be chosen for equal sets. *)

  val print:
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

(** Functor building an implementation of the set structure
    given a totally ordered type. *)
module Make(Ord : Set.OrderedType) : S with type elt = Ord.t 
                                        and module Ord=Ord

module Compare : sig
  val split : ('a -> 'a -> int) -> 'a -> 'a t -> 'a t * bool * 'a t
  (** Meant to be internal, but exporting needed for Mappe.maptoset. *)
  val add : ('a -> 'a -> int) -> 'a -> 'a t -> 'a t
  val mem : ('a -> 'a -> int) -> 'a -> 'a t -> bool
  val remove : ('a -> 'a -> int) -> 'a -> 'a t -> 'a t
  val union : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val diff : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val equal : ('a -> 'a -> int) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val subset : ('a -> 'a -> int) -> 'a t -> 'a t -> bool
  val filter : ('a -> 'a -> int) -> ('a -> bool) -> 'a t -> 'a t
  val partition :
    ('a -> 'a -> int) -> ('a -> bool) -> 'a t -> 'a t * 'a t
end

