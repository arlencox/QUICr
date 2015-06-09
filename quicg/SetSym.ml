
type t = int

let zero = 0

let succ a = a + 1

let pred a = a - 1

let add a b = a + b

let sub a b = a - b

let fmt ff v = Format.pp_print_int ff v

let compare a b = compare a b

let rec fold_seq f starti endi result =
  if starti = endi then
    result
  else
    let result' = f starti result in
    if starti < endi then
      fold_seq f (starti+1) endi result'
    else
      fold_seq f (starti-1) endi result'

let of_int i = i
let to_int i = i
