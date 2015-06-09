open Types
  
module SMap = Mapx.Make(struct
  type t = string
  let compare = compare
end)

module SSet = Set.Make(struct
  type t = string
  let compare = compare
end)
    
module IMap = Mapx.Make(struct
  type t = int
  let compare = compare
end)

module ISet = Set.Make(struct
  type t = int
  let compare = compare
end)

exception Parse_error of Lexing.position * Lexing.position * string
exception Lexing_error of Lexing.position * string
exception Type_error of Lexing.position * Lexing.position * string
exception Invalid_argument of string

type position = Lexing.position * Lexing.position

let null_position =
  let blank = {
    Lexing.pos_fname = "";
    Lexing.pos_lnum = -1;
    Lexing.pos_bol = -1;
    Lexing.pos_cnum = -1;
  } in
  (blank, blank)

type variable = string

let fmt_location ff l =
  Format.fprintf ff "%d:%d" l.Lexing.pos_lnum (l.Lexing.pos_cnum - l.Lexing.pos_bol + 1)

let fmt_position ff (l1,l2) =
  Format.fprintf ff "(%a-%a)" fmt_location l1 fmt_location l2
