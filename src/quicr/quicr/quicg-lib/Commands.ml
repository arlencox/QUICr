open Common
  
type command_name_t =
| SubSetCommand    (* result, set, value *)
| UnionSetCommand  (* result, operand+ *)
| InterSetCommand  (* result, operand+ *)
| ChooseCommand    (* result, set *)
| AssignSetCommand (* result, operand *)
| KillSetCommand   (* variables to kill* *)
| SingletonSetCommand  (* result, base_element *)
| EmptySetCommand  (* result *)
| AddCommand       (* result, operand+ *)
| NegCommand       (* result, operand *)
| AssignNumCommand (* result, operand *)
| KillNumCommand   (* variables to kill* *)
| ConstCommand of Const.const (* result *)
type guard_name_t =
| SubSetEqGuard   (* set1, set2 *)
| SetEqGuard      (* set1, set2 *)
| LessThanGuard   (* operand, operand *)
| LessEqGuard     (* operand, operand *)
| EqualGuard      (* operand, operand *)
| NotEqualGuard   (* operand, operand *)


type command_t = command_name_t * position * variable list
type command0_t = command_name_t * position * int list

type guard_t = guard_name_t * position * variable list
type guard0_t = guard_name_t * position * int list

module GSet = Setx.Make(struct
  type t = guard_t
  let compare = compare
end)

type guard_set_t = GSet.t

type command_list_t = command_t list

type guard_list_t = guard_t list


type block_t = {
  assertions: guard_list_t;
  guards: guard_set_t;
  commands: command_list_t;
}

type control_t =
| IfControl of control_t * control_t * int
| WhileControl of int * control_t * int
| SeqControl of control_t list
| BlockControl of block_t * int

type program_t = {
  init_id: int;
  variables: Types.t SMap.t;
  program: control_t;
  var_tmp: int ref;
}

