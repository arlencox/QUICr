type 'sym string_expr = [
  | `Var of 'sym
  | `Const of string
]

type 'sym string_cnstr = [
  | `Eq of 'sym string_expr * 'sym string_expr
  | `Ne of 'sym string_expr * 'sym string_expr
  | `And of 'sym string_cnstr * 'sym string_cnstr
  | `Or of 'sym string_cnstr * 'sym string_cnstr
  | `False
  | `True
]

type 'sym num_expr = [
  | `Var of 'sym
  | `Const of int
  | `Plus of 'sym num_expr * 'sym num_expr
  | `Minus of 'sym num_expr * 'sym num_expr
  | `Negate of 'sym num_expr
  | `Multiply of 'sym num_expr * 'sym num_expr
]

type 'sym num_cnstr = [
  | `And of 'sym num_cnstr * 'sym num_cnstr
  | `Eq of 'sym num_expr * 'sym num_expr
  | `LE of 'sym num_expr * 'sym num_expr
  | `Lt of 'sym num_expr * 'sym num_expr
]

type 'sym set_expr = [
  | `Union of 'sym set_expr * 'sym set_expr
  | `Inter of 'sym set_expr * 'sym set_expr
  | `Complement of 'sym set_expr
  | `Var of 'sym
  | `Empty
  | `Universe
]

type ('sym,'se,'bc,'nc) set_cnstr = [
  | `Eq of 'se * 'se
  | `SubsetEq of 'se * 'se
  | `Cardinal of 'nc
  | `ForAll of 'sym * 'sym * 'bc (* (a,b,c)  Forall a in b.  c[a] *)
  | `And of ('sym,'se,'bc,'nc) set_cnstr * ('sym,'se,'bc,'nc) set_cnstr
  | `True
  | `False
]

type buf = Buffer.t

let set_cnstr_string (sym: buf -> 'sym -> unit) (se: buf -> 'se -> unit) (bc: buf -> 'bc -> unit) (nc: buf -> 'nc -> unit) (buf: buf) (c: ('sym,'se,'bc,'nc) set_cnstr) =
  let rec to_string = function
    | `Eq(a,b) ->
      se buf a;
      Buffer.add_string buf " = ";
      se buf b
    | `SubsetEq(a,b) ->
      se buf a;
      Buffer.add_string buf " ⊆ ";
      se buf b
    | `Cardinal c -> nc buf c
    | `ForAll (bv,sv,c) ->
      Buffer.add_string buf "∀";
      sym buf bv;
      Buffer.add_string buf " ∈ ";
      sym buf sv;
      Buffer.add_string buf ". ";
      bc buf c
    | `And (c1,c2) ->
      to_string c1;
      Buffer.add_string buf " ∧ ";
      to_string c2
    | `True ->
      Buffer.add_string buf "true"
    | `False ->
      Buffer.add_string buf "false"
  in
  to_string c

let set_expr_string sym buf (e: 'sym set_expr) =
  let rec to_string buf n e =
    let buf' = Buffer.create 80 in
    let m = match e with
      | `Union (a,b) ->
        to_string buf' 10 a;
        Buffer.add_string buf' "∪";
        to_string buf' 10 b;
        10
      | `Inter(a,b) ->
        to_string buf' 20 a;
        Buffer.add_string buf' "∩";
        to_string buf' 20 b;
        20
      | `Complement a ->
        Buffer.add_string buf' "~";
        to_string buf' 50 a;
        50
      | `Var v ->
        sym buf' v;
        100
      | `Empty ->
        Buffer.add_string buf' "∅";
        100
      | `Universe ->
        Buffer.add_string buf' "𝕌";
        100
    in
    if m > n then
      Buffer.add_buffer buf buf'
    else begin
      Buffer.add_string buf "(";
      Buffer.add_buffer buf buf';
      Buffer.add_string buf ")"
    end
  in
  to_string buf 0 e

let num_expr_string sym buf (e: 'sym num_expr) =
  let rec to_string buf n e =
    let buf' = Buffer.create 80 in
    let m = match e with
      | `Var v ->
        sym buf' v;
        100
      | `Const i ->
        Buffer.add_string buf' (string_of_int i);
        100
      | `Plus (a,b) ->
        to_string buf' 10 a;
        Buffer.add_string buf' "+";
        to_string buf' 10 b;
        10
      | `Minus (a,b) ->
        to_string buf' 10 a;
        Buffer.add_string buf' "-";
        to_string buf' 10 b;
        10
      | `Negate a ->
        Buffer.add_string buf' "-";
        to_string buf' 50 a;
        50
      | `Multiply (a,b) ->
        to_string buf' 30 a;
        Buffer.add_string buf' "*";
        to_string buf' 30 b;
        30
    in
    if m > n then
      Buffer.add_buffer buf buf'
    else begin
      Buffer.add_string buf "(";
      Buffer.add_buffer buf buf';
      Buffer.add_string buf ")"
    end
  in
  to_string buf 0 e

let num_expr_string sym buf (e: 'sym num_expr) =
  let rec to_string buf n e =
    let buf' = Buffer.create 80 in
    let m = match e with
      | `Var v ->
        sym buf' v;
        100
      | `Const i ->
        Buffer.add_string buf' (string_of_int i);
        100
      | `Plus (a,b) ->
        to_string buf' 10 a;
        Buffer.add_string buf' "+";
        to_string buf' 10 b;
        10
      | `Minus (a,b) ->
        to_string buf' 10 a;
        Buffer.add_string buf' "-";
        to_string buf' 10 b;
        10
      | `Negate a ->
        Buffer.add_string buf' "-";
        to_string buf' 50 a;
        50
      | `Multiply (a,b) ->
        to_string buf' 30 a;
        Buffer.add_string buf' "*";
        to_string buf' 30 b;
        30
    in
    if m > n then
      Buffer.add_buffer buf buf'
    else begin
      Buffer.add_string buf "(";
      Buffer.add_buffer buf buf';
      Buffer.add_string buf ")"
    end
  in
  to_string buf 0 e

let num_cnstr_string sym buf (c: 'sym num_cnstr) =
  let rec to_string = function
    | `And(c1,c2) ->
      to_string c1;
      Buffer.add_string buf " ∧ ";
      to_string c2
    | `Eq(a,b) ->
      num_expr_string sym buf a;
      Buffer.add_string buf " = ";
      num_expr_string sym buf a;
    | `LE(a,b) ->
      num_expr_string sym buf a;
      Buffer.add_string buf " ≤ ";
      num_expr_string sym buf a;
    | `Lt(a,b) ->
      num_expr_string sym buf a;
      Buffer.add_string buf " < ";
      num_expr_string sym buf a;
    in
  to_string c

let string_expr_string sym buf : 'sym string_expr -> unit = function
  | `Var v -> sym buf v
  | `Const c -> 
    Buffer.add_string buf "\"";
    Buffer.add_string buf (String.escaped c);
    Buffer.add_string buf "\""


let string_cnstr_string sym buf (e: 'sym string_cnstr) =
  let rec to_string buf n e =
    let buf' = Buffer.create 80 in
    let m = match e with
      | `Eq(a,b) ->
        string_expr_string sym buf a;
        Buffer.add_string buf " = ";
        string_expr_string sym buf b;
        100
      | `Ne(a,b) ->
        string_expr_string sym buf a;
        Buffer.add_string buf " ≠ ";
        string_expr_string sym buf b;
        100

      | `And(a,b) ->
        to_string buf' 50 a;
        Buffer.add_string buf " ∧ ";
        to_string buf' 50 b;
        50
      | `Or(a,b) ->
        to_string buf' 30 a;
        Buffer.add_string buf " ∨ ";
        to_string buf' 30 b;
        30
      | `False ->
        Buffer.add_string buf' "false";
        100
      | `True ->
        Buffer.add_string buf' "true";
        100
    in
    if m > n then
      Buffer.add_buffer buf buf'
    else begin
      Buffer.add_string buf "(";
      Buffer.add_buffer buf buf';
      Buffer.add_string buf ")"
    end
  in
  to_string buf 0 e
