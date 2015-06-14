type t =
  | Set
  | Number
  | Bool

type var_type = t

module TSet = Set.Make(struct
  type t = var_type
  let compare = compare
end)

let all = (TSet.add Set (TSet.singleton Number))

let fmt ff = function
  | Number -> Format.fprintf ff "num"
  | Set -> Format.fprintf ff "set"
  | Bool -> Format.fprintf ff "bool"

let toString = function
  | Number -> "num"
  | Set -> "set"
  | Bool -> "bool"
