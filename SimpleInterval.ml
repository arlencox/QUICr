type bound =
  | Num of int
  | Univ

let (<) a b =
  match a, b with
  | Num _, Univ -> true
  | Univ, Num _ -> false
  | Univ, Univ -> false
  | Num a, Num b -> a < b

let (<=) a b =
  match a, b with
  | Num _, Univ -> true
  | Univ, Num _ -> false
  | Univ, Univ -> true
  | Num a, Num b -> a <= b

let (>) a b = b < a
let (>=) a b = b <= a

let (+) a b =
  match a, b with
  | Univ, _
  | _, Univ -> Univ
  | Num a, Num b -> Num (a + b)

let (-) a b =
  match a, b with
  | Univ, Num _ -> Univ
  | _, Univ -> Num 0
  | Num a, Num b -> Num (a - b)

let min a b = if a < b then a else b

let max a b = if a > b then a else b

(* meaning of this domain is the conjunction of the inclusion and the exclusion *)
type t = {
  inc: bound * bound; (* range of elements included in the set *)
  exc: bound * bound; (* range of elements not included in the set *)
}

let top = {
  inc = (Num 0, Univ);
  exc = (Num 0, Univ);
}

let bottom = {
  inc = (Univ, Num 0);
  exc = (Univ, Num 0);
}

let is_bottom t =
  (fst t.inc) > (snd t.inc) || (fst t.exc) > (snd t.exc)


let singleton = {
  inc = (Num 1, Num 1);
  exc = (Num 0, Univ); (* can't represent singleton in exclusion, so this is top *)
}

let empty = {
  inc = (Num 0, Num 0);
  exc = (Univ, Univ);
}

let const c =
  if c = 0 then
    empty
  else {
    inc = (Num c, Num c);
    exc = (Num 0, Univ);
  }

let join_interval (a,b) (c,d) =
  (min a c, max c d)

let widening_interval (a,b) (c,d) =
  (min a c, if d <= b then b else Univ)

let meet_interval (a,b) (c,d) =
  (max a c, min b d)

let le_interval (a,b) (c,d) =
  a >= c && b <= d

let join a b =
  {
    inc = join_interval a.inc b.inc;
    exc = join_interval a.exc b.exc;
  }

let widening a b =
  {
    inc = widening_interval a.inc b.inc;
    exc = widening_interval a.exc b.exc;
  }

let meet a b =
  {
    inc = meet_interval a.inc b.inc;
    exc = meet_interval a.exc b.exc;
  }

let le a b =
  le_interval a.inc b.inc && le_interval a.exc b.exc

let complement a =
  {
    inc = a.exc;
    exc = a.inc;
  }

let union a b =
  (* TODO: this skips reductions between inc and exc *)
  {
    inc = (max (fst a.inc) (fst b.inc), (snd a.inc) + (snd b.inc));
    (* exc computation:
      |u| - b <= |s1| <= |u| - a
      |u| - d <= |s2| <= |u| - c
      --------------------------
      max (|u| - b) (|u| - d) <= |s1| <= min ((|u| - a) + (|u| - c)) |u|
      |u| - (min b d) <= |s1| <=  max (2*|u| - a - c) |u|
      |u| - (min b d) <= |s1| <=  max (|u| - (-|u| + a + c)) (|u| - 0)
      |u| - (min b d) <= |s1| <=  |u| - min (-|u| + a + c)) 0
    *)
    exc = (Num 0, min (snd a.exc) (snd b.exc));
  }

let intersection a b =
  (* TODO: this skips reductions between inc and exc *)
  {
    inc = (Num 0, min (snd a.inc) (snd b.inc));
    (* exc computation:
      |u| - b <= |s1| <= |u| - a
      |u| - d <= |s2| <= |u| - c
      --------------------------

      0 <= |s1| <= min (|u| - a) (|u| - c)
      |u| - |u| <= |s1| <= |u| - (max a c)
    *)
    exc = (max (fst a.exc) (fst b.exc), Univ);
  }

