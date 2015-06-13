module D : Interface.Domain
  with type sym = int
   and type cnstr = int LogicSymbolicSet.t
   and type output = int LogicSymbolicSet.t
   and type query = int LogicSymbolicSet.q
  = BDDFull.Domain


let set_domain config =
  match Selector.Select.process config with
  | Selector.DomainBuilder.SetDomain d -> d
  | _ ->
    failwith "Non-domain returned"
    
let help_string = Selector.DomainBuilder.help_string
