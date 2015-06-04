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
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "->" { ARROW }
  | ";"  { SEMI }
  | "let" { LET }
  | "top" { TOP }
  | "bottom" { BOTTOM }
  | "constrain" { CONSTRAIN }
  | "join" { JOIN }
  | "widening" { WIDENING }
  | "meet" { MEET }
  | "forget" { FORGET }
  | "rename" { RENAME }
  | "sat" { SAT }
  | "le" { LE }
  | "is_bottom" { IS_BOTTOM }
  | "is_top" { IS_TOP }
  | "\\" { DIFF }
  | "U" { UNION }
  | "U+" { DISJUNION }
  | "^" { INTER }
  | "<=" { SUBSET }
  | "=" { EQUAL }
  | "~" { SQUIGGLE }
  | "in" { IN }
  | "/\\" { AND }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']* as i { IDENT i }
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | "//" { comment lexbuf }                                                        
  | eof { EOF }
  | _ { raise Parsing.Parse_error }
and comment = parse
  | eof { EOF }
  | "\n" { incr_linenum lexbuf; token lexbuf }
  | _ { comment lexbuf }    
