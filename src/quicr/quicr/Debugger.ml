module S = LogicSymbolicSet

module Make(D: Interface.Domain
                  with type sym = int
                   and type cnstr = int S.t
                   and type output = int S.t
                   and type query = int S.q
                 )
  : Interface.Domain
    with type sym = int
     and type cnstr = int S.t
     and type output = int S.t
     and type query = int S.q
= struct

  type ctx = D.ctx

  type t = D.t

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let init = D.init

  let top = D.top

  let bottom = D.bottom 

  let symbols = D.symbols

  let constrain = D.constrain

  let serialize = D.serialize

  let sat = D.sat

  let join = D.join

  let widening = D.widening

  let meet = D.meet

  let le = D.le

  let is_top = D.is_top

  let is_bottom = D.is_bottom

  let forget = D.forget

  let rename_symbols = D.rename_symbols

  let query = D.query

  let combine = D.combine

  let pp_print pp_sym ff t =
    D.pp_debug pp_sym ff t

  let pp_debug pp_sym ff t =
    D.pp_print pp_sym ff t


end
