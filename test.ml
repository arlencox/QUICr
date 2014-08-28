open BDDBasic
  
open StringConst
(*open CardinalInterval*)

module Sym = String
module StrC = StringConst.Make(Sym)
module SetD = BDDBasic.Make(Sym)(StrC)

let _ =
  let ctx = SetD.init () in
  let t = SetD.top ctx ["a"; "b"] in
  let b = SetD.bottom ctx ["a"; "b"] in
  assert(SetD.sat b `True);
  assert(SetD.sat t `True);
  assert(SetD.sat b `False);
  ()
