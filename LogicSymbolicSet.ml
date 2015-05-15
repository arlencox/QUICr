type 'sym e =
  | Empty
  | Universe
  | DisjUnion of 'sym e * 'sym e
  | Union of 'sym e * 'sym e
  | Inter of 'sym e * 'sym e
  | Diff of 'sym e * 'sym e
  | Comp of 'sym e
  | Var of 'sym
  | Sing of 'sym

type 'sym t =
  | Eq of 'sym e * 'sym e
  | SubEq of 'sym e * 'sym e
  | In of 'sym * 'sym e
  | And of 'sym t * 'sym t
  | Not of 'sym t
  | True
  | False

type 'sym q = {
  get_eqs: unit -> ('sym * 'sym) list;
  get_eqs_sym: 'sym -> 'sym list;
  (*get_representative: 'sym -> 'sym;*)
}

let prec_e = function
  | Union _ -> 60
  | Inter _ -> 70
  | Diff _ -> 80
  | DisjUnion _ -> 90
  | Comp _ -> 95
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
  | Comp a -> Format.fprintf ff "~%a" pprec a
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
  | Not a -> Format.fprintf ff "(not %a)" (pp pp_sym) a
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

let rec map_sym_e f = function
  | Empty -> Empty
  | Universe -> Universe
  | DisjUnion (a,b) -> DisjUnion(map_sym_e f a, map_sym_e f b)
  | Union (a,b) -> Union(map_sym_e f a, map_sym_e f b)
  | Inter (a,b) -> Inter(map_sym_e f a, map_sym_e f b)
  | Diff (a,b) -> Diff(map_sym_e f a, map_sym_e f b)
  | Comp a -> Comp (map_sym_e f a)
  | Var v -> f false v
  | Sing v -> f true v

let rec map_sym f = function
  | Eq(a,b) -> Eq(map_sym_e f a, map_sym_e f b)
  | SubEq(a,b) -> SubEq(map_sym_e f a, map_sym_e f b)
  | In(a,b) ->
    begin match f true a with
      | Sing a -> In(a, map_sym_e f b)
      | a -> SubEq(a, map_sym_e f b)
    end
  | And(a,b) -> And(map_sym f a, map_sym f b)
  | Not a -> Not (map_sym f a)
  | True -> True
  | False -> False

let map_symbol_e f = map_sym_e (fun is_sing v -> if is_sing then Sing (f v) else Var (f v))

let map_symbol f = map_sym (fun is_sing v -> if is_sing then Sing (f v) else Var (f v))

let rec normalize_e is_comp : 'sym e -> 'sym e * 'sym t = function
  | Empty ->
    ((if is_comp then Universe else Empty), True)
  | Universe ->
    ((if is_comp then Empty else Universe), True)
  | DisjUnion (a,b) ->
    let (a,ca) = normalize_e is_comp a in
    let (b,cb) = normalize_e is_comp b in
    let e = if is_comp then
        Inter(a,b)
      else
        Union(a,b)
    in
    (e,And(And(ca,cb), Eq(Inter(a,b), Empty)))
  | Union (a,b) ->
    let (a,ca) = normalize_e is_comp a in
    let (b,cb) = normalize_e is_comp b in
    if is_comp then 
      (Inter(a,b), And(ca,cb))
    else
      (Union(a,b), And(ca,cb))
  | Inter (a,b) ->
    let (a,ca) = normalize_e is_comp a in
    let (b,cb) = normalize_e is_comp b in
    if is_comp then 
      (Union(a,b), And(ca,cb))
    else
      (Inter(a,b), And(ca,cb))
  | Diff (a,b) ->
    normalize_e is_comp (Inter (a, Comp b))
  | Comp a -> normalize_e (not is_comp) a
  | Var v ->
    if is_comp then
      (Comp (Var v), True)
    else
      (Var v, True)
  | Sing v -> 
    if is_comp then
      (Comp (Sing v), True)
    else
      (Sing v, True)

let normalize_e e = normalize_e false e

let rec normalize = function
  | Eq(a,b) ->
    let a,ca = normalize_e a in
    let b,cb = normalize_e b in
    And(Eq(a,b),And(ca,cb))
  | SubEq(a,b) ->
    let a,ca = normalize_e a in
    let b,cb = normalize_e b in
    And(SubEq(a,b),And(ca,cb))
  | In(a,b) ->
    let b,cb = normalize_e b in
    And(SubEq(Sing a, b), cb)
  | And(a,b) ->
    And (normalize a, normalize b)
  | Not a -> Not (normalize a)
  | True -> True
  | False -> False
