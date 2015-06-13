let process config =
  let lexbuf = Lexing.from_string config in
  let result = try
      Parser.construct Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "(%d:%d) Domain config error@."
          pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  in
  result

