{
open Lexing
open Parser

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
                                Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                                Lexing.pos_bol = pos.Lexing.pos_cnum;
                              }
}

rule token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | "\n" { incr_linenum lexbuf; token lexbuf }
  | "<" { LB }
  | ">" { RB }
  | "help" { HELP }
  | "true" { TRUE }
  | "false" { FALSE }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '-' '0'-'9']* as i { IDENT i }
  | ("-"? ['0'-'9']+) as i { INT i }
  | "'" { tok_string (Buffer.create 80) lexbuf }
  | "//" { comment lexbuf }                                                        
  | eof { EOF }
  | _ { raise Parsing.Parse_error }
and comment = parse
  | eof { EOF }
  | "\n" { incr_linenum lexbuf; token lexbuf }
  | _ { comment lexbuf }    
and tok_string b = parse
  | eof { raise Parsing.Parse_error }
  | "'" { STRING (Buffer.contents b) }
  | "\\'" { Buffer.add_string b "'"; tok_string b lexbuf }
  | _ as c { Buffer.add_char b c; tok_string b lexbuf }
