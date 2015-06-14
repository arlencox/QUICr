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
  | ";" { SEMI }
  | "branch" { BRANCH }
  | "both" { BOTH }
  | "and" { AND }
  | "loop" { LOOP }
  | "rename" { RENAME }
  | "if" { IF }
  | "while" { WHILE }
  | "kill" { KILL }
  | "else" { ELSE }
  | "choose" { CHOOSE }
  | "for" { FOR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "\\" { DIFF }
  | "U" { UNION }
  | "U+" { DISJUNION }
  | "^" { INTER }
  | "<=" { SUBSET }
  | "=" { EQUAL }
  | "~" { SQUIGGLE }
  | "*" { NONDET }
  | "in" { IN }
  | "assert" { ASSERT }
  | "assume" { ASSUME }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']* as i { IDENT i }
  | "//" { comment lexbuf }                                                        
  | eof { EOF }
  | _ { raise Parsing.Parse_error }
and comment = parse
  | eof { EOF }
  | "\n" { incr_linenum lexbuf; token lexbuf }
  | _ { comment lexbuf }    
