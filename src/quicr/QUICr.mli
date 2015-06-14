module Logic : sig module SymbolicSet = QUICr_Internal.LogicSymbolicSet end

module type I = QUICr_Internal.Interface.Domain

module D : I with type sym = int
              and type cnstr = int Logic.SymbolicSet.t
              and type output = int Logic.SymbolicSet.t
              and type query = int Logic.SymbolicSet.q

val set_domain : string -> (module I
                             with type sym = int
                              and type cnstr = int Logic.SymbolicSet.t
                              and type output = int Logic.SymbolicSet.t
                              and type query = int Logic.SymbolicSet.q)

module Rename = QUICr_Internal.Rename

val help_string : string
