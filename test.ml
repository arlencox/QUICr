open BDDBasic
  
open StringConst
(*open CardinalInterval*)

module Sym = struct
  include String
  let to_string s = s
end
module StrC = StringConst.Make(Sym)
module SetD = BDDBasic.Make(Sym)(StrC)

let pp ff t =
  let cnstr = SetD.to_constraint t in
  let buf = Buffer.create 80 in
  let sym = Buffer.add_string in
  Commands.set_cnstr_string
    sym
    (Commands.set_expr_string sym)
    (Commands.string_cnstr_string sym)
    (Commands.num_cnstr_string sym)
    buf
    cnstr;
  let str = Buffer.contents buf in
  Format.pp_print_string ff str

let _ =
  let ctx = SetD.init () in
  let t = SetD.top ctx ["a"; "b"] in
  let b = SetD.bottom ctx ["a"; "b"] in
  assert(SetD.sat b `True);
  assert(SetD.sat t `True);
  assert(SetD.sat b `False);

  (* try a basic constraint *)
  let t = SetD.constrain (`SubsetEq(`Var "a", `Var "b")) t in
  Format.printf "basic constraint:@.";
  Format.printf "%a@." pp t;

  (* extend environment *)
  let t = SetD.add_symbols ["c"] t in
  Format.printf "extended environment:@.";
  Format.printf "%a@." pp t;

  (* additional constraint *)
  let t = SetD.constrain (`SubsetEq(`Var "b", `Var "c")) t in
  Format.printf "additional constraint:@.";
  Format.printf "%a@." pp t;

  (* add an equality *)
  let t = SetD.constrain (`SubsetEq(`Var "c", `Var "b")) t in
  Format.printf "equality:@.";
  Format.printf "%a@." pp t;

  (* extend environment again and add a c = d *)
  let t = SetD.add_symbols ["d"] t in
  let t = SetD.constrain (`Eq(`Var "c", `Var "d")) t in
  Format.printf "another equality:@.";
  Format.printf "%a@." pp t;

  (* project out c *)
  let t = SetD.forget ["c"] t in
  Format.printf "forget c:@.";
  Format.printf "%a@." pp t;

  (* check satisfiability *)
  Format.printf "check if implies b = d:@.";
  Format.printf "%b@." (SetD.sat t (`Eq(`Var "b", `Var "d")));

  (* create new domain *)
  let t' = SetD.top ctx (SetD.symbols t) in
  Format.printf "new domain:@.";
  Format.printf "%a@." pp t';

  (* constrain new domain *)
  let t' = SetD.constrain (`Eq(`Var "b", `Var "d")) t' in
  Format.printf "new domain b = d:@.";
  Format.printf "%a@." pp t';

  (* join the two domains *)
  let t'' = SetD.join ["b","b","a"; "d","d","c"] t t' in
  Format.printf "joined:@.";
  Format.printf "%a@." pp t'';

  (* meet the two domains *)
  let t'' = SetD.meet ["b","d","b"; "d","c","d"] t t' in
  Format.printf "met:@.";
  Format.printf "%a@." pp t'';

  
  ()
