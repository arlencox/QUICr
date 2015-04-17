type 'sym e =
  | Empty
  | Universe
  | DisjUnion of 'sym e * 'sym e
  | Union of 'sym e * 'sym e
  | Inter of 'sym e * 'sym e
  | Diff of 'sym e * 'sym e
  | Var of 'sym
  | Sing of 'sym

type 'sym t =
  | Eq of 'sym e * 'sym e
  | SubEq of 'sym e * 'sym e
  | In of 'sym * 'sym e
  | And of 'sym t * 'sym t
  | True
  | False

type 'sym q = {
  get_eqs: unit -> ('sym * 'sym) list;
  get_eqs_sym: 'sym -> 'sym list;
}

let prec_e = function
  | Union _ -> 60
  | Inter _ -> 70
  | Diff _ -> 80
  | DisjUnion _ -> 90
  | Var _
  | Sing _
  | Empty
  | Universe -> 100

let rec pp_noparen_e pp_sym ff t =
  let pprec = pp_e ~prec:(prec_e t) pp_sym in
  match t with
  | Empty -> Format.fprintf ff "∅"
  | Universe -> Format.fprintf ff "𝕌"
  | DisjUnion (a,b) -> Format.fprintf ff "%a ⊎ %a" pprec a pprec b
  | Union (a,b) -> Format.fprintf ff "%a ∪ %a" pprec a pprec b
  | Inter (a,b) -> Format.fprintf ff "%a ∩ %a" pprec a pprec b
  | Diff (a,b) -> Format.fprintf ff "%a ∖ %a" pprec a pprec b
  | Var v -> pp_sym ff v
  | Sing v -> Format.fprintf ff "{%a}" pp_sym v

and pp_e ?prec:(p=0) pp_sym ff t =
  if prec_e t < p then
    Format.fprintf ff "@[<hv 2>(%a)@]" (pp_noparen_e pp_sym) t
  else
    pp_noparen_e pp_sym ff t

let rec pp pp_sym ff t =
  let ppe = pp_e pp_sym in
  match t with
  | Eq (a,b) -> Format.fprintf ff "%a = %a" ppe a ppe b
  | SubEq (a,b) -> Format.fprintf ff "%a ⊆ %a" ppe a ppe b
  | In (a,b) -> Format.fprintf ff "%a ∈ %a" pp_sym a ppe b
  | And (a,b) -> Format.fprintf ff "%a ∧ %a" (pp pp_sym) a (pp pp_sym) b
  | True -> Format.fprintf ff "true"
  | False -> Format.fprintf ff "false"

let to_string_e pp_sym e =
  let b = Buffer.create 80 in
  let ff = Format.formatter_of_buffer b in
  pp_e pp_sym ff e;
  Format.pp_print_flush ff ();
  Buffer.contents b

let to_string pp_sym t =
  let b = Buffer.create 80 in
  let ff = Format.formatter_of_buffer b in
  pp pp_sym ff t;
  Format.pp_print_flush ff ();
  Buffer.contents b

