open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    ocaml_lib ~tag_name:"use_quicr" "quicr/QUICr"
  | _ -> ()
end
