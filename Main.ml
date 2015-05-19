module type SetDom = Interface.Domain
  with type sym = int
   and type cnstr = int LogicSymbolicSet.t
   and type output = int LogicSymbolicSet.t
   and type query = int LogicSymbolicSet.q

let domain : (module SetDom) list ref = ref []

let push (d : (module SetDom)) =
  domain := d :: !domain

let pop () =
  match !domain with
  | hd::rest ->
    domain := rest;
    hd
  | [] ->
    Printf.fprintf stderr "Error: domain combinator did not have a domain to use as input\n%!";
    exit 1

let arg_blank = ("", Arg.Unit (fun () -> ()), " ")

(** list of abstract domains to enable *)
let domains = [
  ("-bdd-full", Arg.Unit (fun () -> push (module BDDFull.Domain)), " Full BDD-based domain");
]

(** list of domain combinators to enable *)
let combinators = [

  (* logger combinator *)
  ("-logger", Arg.String (fun s ->
       let module L = (struct
         let file = s
       end) in
       let module D = (val pop ()) in
       let module Log = Logger.Domain.Make(L)(D) in
       push (module Log)
     ), "<file> Log domain interactions to file");

  (* packer combinator *)
  ("-pack", Arg.Unit (fun () ->
       let module D = (val pop ()) in
       let module P = Packer.Domain.Make(D) in
       push (module P)
     ), " Pack variables into multiple domain instances");

  (* equality combinator *)
  ("-eq", Arg.Unit (fun () ->
       let module D = (val pop ()) in
       let module P = Eq.Domain.Make(D) in
       push (module P)
     ), " Track equality constraints externally from the domain");

  (* debugger combinator *)
  ("-debug", Arg.Unit (fun () ->
       let module D = (val pop ()) in
       let module P = DebugPrint.Domain.Make(D) in
       push (module P)
     ), " Use debug printer");

  (* singleton handler *)
  ("-sing", Arg.Unit (fun () ->
       let module D = (val pop ()) in
       let module P = Sing.Domain.Make(D) in
       push (module P)
     ), " Support singleton sets");
]

let usage =
  "Usage: sdsl <domain config> [file]\n" ^
  "  <domain config> indicates which domain should be run.  Domains can be\n" ^
  "  specified as below.  Each operation manipulates a stack of domains.\n" ^
  "  Basic domains, push a new domain onto the stack.  Combinators consume\n" ^
  "  domains off the stack and produce new domains.  At least one domain must\n" ^
  "  be specified.  Extra domains on the stack are ignored and will issue a\n" ^
  "  warning.\n"


let stream = ref ((fun () -> ()),stdin)

let get_stream fname =
  let fin = open_in fname in
  stream := ((fun () -> close_in fin), fin)



let run () =
  (* assemble command line arguments *)
  let args = 
    domains @
    [arg_blank] @
    combinators @
    [arg_blank] @
    SDSL.Interp.args @
    [arg_blank]
  in
  let args = Arg.align args in

  begin try
      (* parse environment args *)
      let envargv = try Sys.getenv "SDSLPARAMS" |>
                        Str.split (Str.regexp " +") |>
                        Array.of_list
        with Not_found -> [| |] in

      (* add environment args to command line arguments *)
      let argv = Array.append Sys.argv envargv in

      (* parse arguments *)
      Arg.current := 0;
      Arg.parse_argv argv args get_stream usage;
    with
    | Arg.Help s ->
      Printf.fprintf stderr "%s\n%!" s;
      exit 1
    | Arg.Bad s ->
      Printf.fprintf stderr "%s\n\n%s%!" s (Arg.usage_string args usage);
      exit 1
  end;
    
  (* build abstract interpreter *)
  let d : (module SetDom) = match !domain with
    | [d] -> d
    | d::_ ->
      Printf.fprintf stderr "Warning: Unused domain(s) on domain stack\n%!";
      d
    | [] ->
      Printf.fprintf stderr "Error: Domain stack is empty, please specify an abstract domain\n\n%s%!" (Arg.usage_string args usage);
      exit 1
  in
  let module D = (val d) in
  let module I = SDSL.Interp.Make(D) in

  (* do parsing *)
  let (close,stream) = !stream in
  let lexbuf = Lexing.from_channel stream in
  let ast = try
      SDSL.Parser.program SDSL.Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "(%d:%d) Parse error@."
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  in

  (* run the analysis *)
  I.interpret ast;

  (* clean up *)
  close ()



let _ =
  run ()
