
%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token ARROW
%token SEMI

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
%token RENAME
%token LET
%token TOP
%token BOTTOM
%token CONSTRAIN
%token JOIN
%token WIDENING
%token MEET
%token FORGET
%token SAT
%token LE
%token IS_BOTTOM
%token IS_TOP
%token <int> INT
%token AND
%token TRUE
%token FALSE
%token NOT

%left AND
%left NOT

%left SEMI
%left DIFF
%left UNION DISJUNION
%left INTER
%left SQUIGGLE

%{
  module L = LogicSymbolicSet
  open AST

%}


%start trace
%type <AST.t list> trace

%%

trace
  : statement_list EOF { $1 }
  ;

statement_list
  : { [] }
  | statement statement_list { $1::$2 }
  ;

statement
  : LET IDENT EQUAL trans { Trans ($2,$4) }
  | SAT IDENT cond { Query (Sat ($2,$3)) }
  | LE IDENT IDENT { Query (Le ($2,$3)) }
  | IS_BOTTOM IDENT { Query (IsBottom $2) }
  | IS_TOP IDENT { Query (IsTop $2) }
  ;

trans
  : TOP { Top }
  | BOTTOM { Bottom }
  | CONSTRAIN cond IDENT { Constrain($2,$3) }
  | JOIN IDENT IDENT { Join($2,$3) }
  | WIDENING IDENT IDENT { Widening($2,$3) }
  | MEET IDENT IDENT { Meet($2,$3) }
  | FORGET ident_list IDENT { Forget($2,$3) }
  | RENAME LSQUARE rename_list_opt RSQUARE IDENT { Rename($3,$5) }
  ;

cond
  : expr EQUAL expr { L.Eq ($1, $3) }
  | expr SUBSET expr { L.SubEq ($1, $3) }
  | INT IN expr { L.In ($1, $3) }
  | cond AND cond { L.And ($1, $3) }
  | NOT cond { L.Not $2 }
  | TRUE { L.True }
  | FALSE { L.False }
  ;

expr
  : INT { L.Var $1 }
  | LCURLY RCURLY { L.Empty }
  | LCURLY INT RCURLY { L.Sing $2 }
  | expr UNION expr { L.Union ($1,$3) }
  | expr DISJUNION expr { L.DisjUnion ($1,$3) }
  | expr INTER expr { L.Inter ($1,$3) }
  | expr DIFF expr { L.Diff ($1, $3) }
  | SQUIGGLE expr { L.Comp $2 }
  | LPAREN expr RPAREN { $2 }
  ;

ident_list
  :  { [] }
  | INT ident_list { $1::$2 }
  ;

rename_list_opt
  : { [] }
  | rename_list { $1 }
  ;

rename_list
  : INT ARROW INT { [($1,$3)] }
  | rename_list SEMI rename_list { $1 @ $3 }


