type var = string

module L = QUICr.Logic.SymbolicSet

type t =
  | Skip
  | Seq of t * t
  | Branch of t * t
  | Both of t * t
  | Loop of t
  | Kill of var list
  | Rename of (var * var) list
  | Assign of var * var L.e
  | Choose of var * var L.e
  | Assume of var L.t
  | Assert of int * var L.t
  | For of var * var L.e * t
