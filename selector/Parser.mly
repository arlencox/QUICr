%token LB RB EOF TRUE FALSE HELP
%token <string> IDENT
%token <string> INT
%token <string> STRING

%start construct
%type <DomainBuilder.t> construct

%%

construct
  : domain EOF { $1 }
  | HELP EOF { DomainBuilder.get_help () }
  ;

domain
  : IDENT arg_list { DomainBuilder.build_domain $1 $2 }
  ;

arg_list
  :  { [] }
  | LB args RB { $2 }
  ;

args
  : arg      { [$1] }
  | arg args { $1::$2 }
  ;

arg
  : domain { $1 }
  | TRUE { DomainBuilder.Bool true }
  | FALSE { DomainBuilder.Bool false }
  | STRING { DomainBuilder.String $1 }
  | INT { DomainBuilder.Int (int_of_string $1) }
  ;
