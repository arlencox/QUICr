module type Config = sig
  val reorder : string option
end

module Make : functor(C: Config) ->
  Interface.Domain
  with type sym = int
   and type cnstr = int LogicSymbolicSet.t
   and type output = int LogicSymbolicSet.t
   and type query = int LogicSymbolicSet.q
