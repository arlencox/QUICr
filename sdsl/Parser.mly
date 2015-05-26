%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token SEMI
%token IF
%token WHILE
%token KILL
%token ELSE

%token DIFF
%token UNION
%token DISJUNION
%token INTER
%token SUBSET
%token EQUAL
%token <string> IDENT
%token EOF
%token SQUIGGLE
%token IN
%token NONDET
%token ASSERT
%token ASSUME
%token BRANCH
%token AND
%token RENAME
%token BOTH
%token LOOP


%left SEMI
%left DIFF
%left UNION DISJUNION
%left INTER
%left SQUIGGLE

%{
  module L = LogicSymbolicSet

let mkif c t e =
  match c with
  | None -> AST.Branch(t,e)
  | Some c -> AST.Branch(AST.Seq(AST.Assume c,t), AST.Seq(AST.Assume (L.Not c), e))

let mkwhile c b =
  match c with
  | None ->
    AST.Loop(b)
  | Some c ->
      AST.Seq(
        AST.Loop(
          AST.Seq(
            AST.Assume c,
            b)),
        AST.Assume(L.Not c))

%}


%start program
%type <AST.t> program

%%

program
  : statement EOF { $1 }
  ;

statement
  : { AST.Skip }
  | IDENT EQUAL expr { AST.Assign($1,$3) }
  | KILL IDENT { AST.Kill $2 }
  | RENAME IDENT IDENT { AST.Rename ($2,$3) }
  | statement SEMI statement { AST.Seq($1,$3) }
  | BRANCH LCURLY statement RCURLY ELSE LCURLY statement RCURLY {
      AST.Branch ($3,$7)
    }
  | BOTH LCURLY statement RCURLY AND LCURLY statement RCURLY {
      AST.Both ($3,$7)
    }
  | IF LPAREN cond RPAREN LCURLY statement RCURLY {
      mkif $3 $6 AST.Skip
    }
  | IF LPAREN cond RPAREN LCURLY statement RCURLY ELSE LCURLY statement RCURLY {
      mkif $3 $6 $10
    }
  | WHILE LPAREN cond RPAREN LCURLY statement RCURLY {
      mkwhile $3 $6
    }
  | LOOP LCURLY statement RCURLY {
      AST.Loop($3)
    }
  | ASSERT LPAREN cond RPAREN {
      let c = match $3 with
        | None -> L.False
        | Some c -> c
      in
      AST.Assert c
    }
  | ASSUME LPAREN cond RPAREN {
      let c = match $3 with
        | None -> L.True
        | Some c -> c
      in
      AST.Assume c
    }
  ;

cond
  : expr EQUAL expr { Some (L.Eq ($1, $3)) }
  | expr SUBSET expr { Some (L.SubEq ($1, $3)) }
  | IDENT IN expr { Some (L.In ($1, $3)) }
  | NONDET { None }
  ;

expr
  : IDENT { L.Var $1 }
  | LCURLY RCURLY { L.Empty }
  | LCURLY IDENT RCURLY { L.Sing $2 }
  | expr UNION expr { L.Union ($1,$3) }
  | expr DISJUNION expr { L.DisjUnion ($1,$3) }
  | expr INTER expr { L.Inter ($1,$3) }
  | expr DIFF expr { L.Diff ($1, $3) }
  | SQUIGGLE expr { L.Comp $2 }
  | LPAREN expr RPAREN { $2 }
  ;

