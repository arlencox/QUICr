module Logic = struct
  module SymbolicSet = QUICr_Internal.LogicSymbolicSet
end

module Rename = QUICr_Internal.Rename

module type I = QUICr_Internal.Interface.Domain

module D : I with type sym = int
              and type cnstr = int Logic.SymbolicSet.t
              and type output = int Logic.SymbolicSet.t
              and type query = int Logic.SymbolicSet.q
  = QUICr_Internal.BDDFull



let set_domain config =
  match QUICr_Internal.Select.process config with
  | QUICr_Internal.DomainBuilder.SetDomain d -> d
  | _ ->
    failwith "Non-domain returned"
    
let help_string = QUICr_Internal.DomainBuilder.help_string
