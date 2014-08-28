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
  | `ForAll of 'sym * 'sym * 'bc
  | `And of ('sym,'se,'bc,'nc) set_cnstr * ('sym,'se,'bc,'nc) set_cnstr
  | `True
  | `False
]


let rec pp_set_cnstr pp_sym pp_se pp_bc pp_nc ff = function
  | `Eq(a,b) -> Format.fprintf ff "%a = %a" pp_se a pp_se b
  | `SubsetEq(a,b) -> Format.fprintf ff "%a ⊆ %a" pp_se a pp_se b
  | `Cardinal c -> pp_nc ff c
  | `Forall (bv,sv,bc) -> Format.fprintf ff "∀%a ∈ %a. %a" pp_sym bv pp_sym sv pp_bc bc
  | `And (c1,c2) -> Format.fprintf ff "%a@ ∧ %a" (pp_set_cnstr pp_sym pp_se pp_bc pp_nc) c1 (pp_set_cnstr pp_sym pp_se pp_bc pp_nc) c2
  | `True -> Format.fprintf ff "true"

(*let rec pp_set_expr pp_sym n ff = function
  | `Union *)
