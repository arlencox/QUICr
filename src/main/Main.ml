let arg_blank = ("", Arg.Unit (fun () -> ()), " ")


let usage =
  "Usage: sdsl [options] [file]\n" ^
  QUICr.help_string

type ft =
  | FT_SDSL
  | FT_STRACE


let stream = ref ((fun () -> ()),stdin,FT_SDSL)

let get_stream fname =
  let l = String.length fname in
  let ft = if (String.sub fname (l-7) 7) = ".strace" then
      FT_STRACE
    else
      FT_SDSL
  in
  let fin = open_in fname in
  stream := ((fun () -> close_in fin), fin, ft)



let run () =
  let dom_str = ref "lin" in
  (* assemble command line arguments *)
  let args = 
    ["-dom", Arg.Set_string dom_str, "<cfg> Configure domain according to cfg"] @
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
  
  let module D = (val (QUICr.set_domain !dom_str)) in
  let module I = SDSL.Interp.Make(D) in
  let module IT = STrace.Interp.Make(D) in

  (* do parsing *)
  let (close,stream,ft) = !stream in
  let lexbuf = Lexing.from_channel stream in
  begin match ft with
  | FT_SDSL ->
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
  | FT_STRACE ->
    let ast = try
        STrace.Parser.trace STrace.Lexer.token lexbuf
      with Parsing.Parse_error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Format.printf "(%d:%d) Parse error@."
          pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        exit 1
    in

    (* run the analysis *)
    IT.interpret ast

  end;

  (* clean up *)
  close ()



let _ =
  run ()
