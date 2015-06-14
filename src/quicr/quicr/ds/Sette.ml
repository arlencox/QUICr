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

(** Sets over ordered types *)

(**  Modified by B. Jeannet to get a generic type and a few additions
     (like conversions form and to maps and pretty-printing). *)

(** Sets are represented by balanced binary trees (the heights of the children
    differ by at most 2 *)

type 'a set =
  | Empty
  | Node of ('a set) * 'a * ('a set) * int

type 'a t = 'a set

let height = function
  | Empty -> 0
  | Node(_, _, _, h) -> h

(** Creates a new node with left son l, value v and right son r.  We must have
    all elements of l < v < all elements of r.  l and r must be balanced and |
    height l - height r | <= 2.  Inline expansion of height for better
    speed. *)
let create l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(** Same as create, but performs one step of rebalancing if necessary.
    Assumes l and r balanced and | height l - height r | <= 3.  Inline
    expansion of create for better speed in the most frequent case where no
    rebalancing is required. *)
let bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node(ll, lv, lr, _) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> invalid_arg "Set.bal"
        | Node(lrl, lrv, lrr, _)->
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node(rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> invalid_arg "Set.bal"
        | Node(rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(** Smallest and greatest element of a set *)
let rec min_elt = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_elt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node(l, v, Empty, _) -> v
  | Node(l, v, r, _) -> max_elt r

(** Remove the smallest element of the given set *)
let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

(** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  Assume | height l - height r | <= 2. *)
let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton x = Node(Empty, x, Empty, 1)

let rec iter f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Empty -> accu
  | Node(l, v, r, _) -> fold f l (f v (fold f r accu))

let rec for_all p = function
    Empty -> true
  | Node(l, v, r, _) -> p v && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node(l, v, r, _) -> p v || exists p l || exists p r

let rec cardinal = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let print
    ?(first : (unit, Format.formatter, unit) format = ("[@[" : (unit, Format.formatter, unit) format))
    ?(sep : (unit, Format.formatter, unit) format = ("@ ":(unit, Format.formatter, unit) format))
    ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
    (print_elt:Format.formatter -> 'a -> unit)
    (formatter:Format.formatter)
    (set:'a t)
  : unit
  =
  Format.fprintf formatter first;
  let firstelt = ref true in
  iter
    (begin fun elt ->
       if !firstelt then firstelt := false else Format.fprintf formatter sep;
       Format.fprintf formatter "%a" print_elt elt
     end)
    set;
  Format.fprintf formatter last

module Compare = struct
  (** Insertion of one element *)
  let rec add (compare:'a -> 'a -> int) x = function
    | Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
      let c = compare x v in
      if c = 0 then t
      else if c < 0 then bal (add compare x l) v r
      else bal l v (add compare x r)

  (** Same as create and bal, but no assumptions are made on the relative
      heights of l and r. *)
  let rec join (compare:'a -> 'a -> int) l v r =
    match (l, r) with
    | (Empty, _) -> add compare v r
    | (_, Empty) -> add compare v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join compare lr v r)
      else if rh > lh + 2 then bal (join compare l v rl) rv rr
      else create l v r

  (** Merge two trees l and r into one.  All elements of l must precede the
      elements of r.  No assumption on the heights of l and r. *)
  let concat (compare:'a -> 'a -> int) t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join compare t1 (min_elt t2) (remove_min_elt t2)

  (** Splitting.  split x s returns a triple (l, present, r) where
      - l is the set of elements of s that are < x
      - r is the set of elements of s that are > x
      - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)
  let rec split (compare:'a -> 'a -> int) x = function
    | Empty ->
      (Empty, false, Empty)
    | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split compare x l in (ll, pres, join compare rl v r)
      else
        let (lr, pres, rr) = split compare x r in (join compare l v lr, pres, rr)

  (** Implementation of the set operations *)
  let rec mem (compare:'a -> 'a -> int) x = function
    | Empty -> false
    | Node(l, v, r, _) ->
      let c = compare x v in
      c = 0 || mem compare x (if c < 0 then l else r)

  let rec remove (compare:'a -> 'a -> int) x = function
    | Empty -> Empty
    | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then merge l r
      else if c < 0 then bal (remove compare x l) v r
      else bal l v (remove compare x r)

  let union (compare:'a -> 'a -> int) s1 s2 =
    let rec union s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
        if h1 >= h2 then
          if h2 = 1 then add compare v2 s1 else begin
            let (l2, _, r2) = split compare v1 s2 in
            join compare (union l1 l2) v1 (union r1 r2)
          end
        else
        if h1 = 1 then add compare v1 s2 else begin
          let (l1, _, r1) = split compare v2 s1 in
          join compare (union l1 l2) v2 (union r1 r2)
        end
    in
    union s1 s2

  let inter (compare:'a -> 'a -> int) s1 s2 =
    let rec inter s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, r1, _), t2) ->
        match split compare v1 t2 with
          (l2, false, r2) ->
          concat compare (inter l1 l2) (inter r1 r2)
        | (l2, true, r2) ->
          join compare (inter l1 l2) v1 (inter r1 r2)
    in
    inter s1 s2

  let diff (compare:'a -> 'a -> int) s1 s2 =
    let rec diff s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, _), t2) ->
        match split compare v1 t2 with
          (l2, false, r2) ->
          join compare (diff l1 l2) v1 (diff r1 r2)
        | (l2, true, r2) ->
          concat compare (diff l1 l2) (diff r1 r2)
    in
    diff s1 s2

  let compare_aux (compare:'a -> 'a -> int) l1 l2 =
    let rec compare_aux l1 l2 =
      match (l1, l2) with
      | ([], []) -> 0
      | ([], _)  -> -1
      | (_, []) -> 1
      | (Empty :: t1, Empty :: t2) ->
        compare_aux t1 t2
      | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
        let c = compare v1 v2 in
        if c <> 0 then c else compare_aux (r1::t1) (r2::t2)
      | (Node(l1, v1, r1, _) :: t1, t2) ->
        compare_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
      | (t1, Node(l2, v2, r2, _) :: t2) ->
        compare_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)
    in
    compare_aux l1 l2

  let equal (compare:'a -> 'a -> int) (s1:'a t) (s2:'a t) =
    s1==s2 || compare_aux compare [s1] [s2] = 0

  let compare (compare:'a -> 'a -> int) s1 s2 =
    if s1==s2 then 0 else compare_aux compare [s1] [s2]

  let subset (compare:'a -> 'a -> int) s1 s2 =
    let rec subset s1 s2 =
      match (s1, s2) with
      | Empty, _ ->
        true
      | _, Empty ->
        false
      | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
        let c = compare v1 v2 in
        if c = 0 then
          subset l1 l2 && subset r1 r2
        else if c < 0 then
          subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
        else
          subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2
    in
    s1==s2 || (subset s1 s2)

  let rec filter (compare:'a -> 'a -> int) p = function
    | Empty -> Empty
    | Node(l, v, r, _) ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter compare p l in
      let pv = p v in
      let r' = filter compare p r in
      if pv then join compare l' v r' else concat compare l' r'

  let rec partition (compare:'a -> 'a -> int) p = function
    | Empty -> (Empty, Empty)
    | Node(l, v, r, _) ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition compare p l in
      let pv = p v in
      let (rt, rf) = partition compare p r in
      if pv
      then (join compare lt v rt, concat compare lf rf)
      else (concat compare lt rt, join compare lf v rf)

end

let add x set = Compare.add Pervasives.compare x set
let mem x set = Compare.mem Pervasives.compare x set
let remove x set = Compare.remove Pervasives.compare x set
let union s1 s2 = Compare.union Pervasives.compare s1 s2
let inter s1 s2 = Compare.inter Pervasives.compare s1 s2
let diff s1 s2 = Compare.diff Pervasives.compare s1 s2
let compare s1 s2 = Compare.compare Pervasives.compare s1 s2
let equal s1 s2 = Compare.equal Pervasives.compare s1 s2
let subset s1 s2 = Compare.subset Pervasives.compare s1 s2
let filter f s = Compare.filter Pervasives.compare f s
let partition f s = Compare.partition Pervasives.compare f s

(** Output signature of the functor {!Sette.Make}. *)
module type S =
sig
  type elt
  type t
  val repr : t -> elt set
  val obj : elt set -> t
  module Ord : (Set.OrderedType with type t=elt)

  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val print:
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

(** Functor building an implementation of the set structure
    given a totally ordered type. *)
module Make(Ord: Set.OrderedType) = struct
  type elt = Ord.t
  type t = elt set
  let repr = Obj.magic
  let obj = Obj.magic

  module Ord = Ord

  let empty = empty
  let is_empty = is_empty
  let singleton = singleton
  let iter =iter
  let fold = fold
  let for_all = for_all
  let exists = exists
  let cardinal = cardinal
  let elements = elements
  let min_elt = min_elt
  let max_elt = max_elt
  let choose = min_elt
  let print = print
  let add = Compare.add Ord.compare
  let mem = Compare.mem Ord.compare
  let remove = Compare.remove Ord.compare
  let union = Compare.union Ord.compare
  let inter = Compare.inter Ord.compare
  let diff = Compare.diff Ord.compare
  let equal = Compare.equal Ord.compare
  let compare = Compare.compare Ord.compare
  let subset = Compare.subset Ord.compare
  let filter = Compare.filter Ord.compare
  let partition = Compare.partition Ord.compare
end
