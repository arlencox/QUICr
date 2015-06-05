module L = LogicSymbolicSet

type s = string

type c =
  | Top
  | Bottom
  | Constrain of int L.t * s
  | Join of s * s
  | Widening of s * s
  | Meet of s * s
  | Forget of int list * s
  | Rename of (int * int) list * s

type p =
  | Sat of s * int L.t
  | Le of s * s
  | IsBottom of s
  | IsTop of s

type t = 
  | Trans of s * c
  | Query of p

let pp_c ff = function
  | Top -> Format.fprintf ff "top"
  | Bottom -> Format.fprintf ff "bottom"
  | Constrain(c,s) -> Format.fprintf ff "constrain %a %s"  (L.pp Format.pp_print_int) c s
  | Join(s1,s2) -> Format.fprintf ff "join %s %s" s1 s2
  | Widening(s1,s2) -> Format.fprintf ff "widening %s %s" s1 s2
  | Meet(s1,s2) -> Format.fprintf ff "meet %s %s" s1 s2
  | Forget(vs,s) -> Format.fprintf ff "forget @[<h>%a@] %s" (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int) vs s
  | Rename(rs,s) ->
    Format.fprintf ff "rename @[<h>[%a]@] %s"
      (Format.pp_print_list ~pp_sep:(fun ff () -> Format.pp_print_string ff "; ") (fun ff (a,b) -> Format.fprintf ff "%d -> %d" a b)) rs
      s

let pp_p ff = function
  | Sat (s,c) -> Format.fprintf ff "sat %s %a"  s (L.pp Format.pp_print_int) c
  | Le (s1,s2) -> Format.fprintf ff "le %s %s" s1 s2
  | IsBottom s -> Format.fprintf ff "is_bottom %s" s
  | IsTop s -> Format.fprintf ff "is_top %s" s

let pp_t ff = function
  | Trans (s,c) -> Format.fprintf ff "let %s = %a" s pp_c c
  | Query p -> pp_p ff p
