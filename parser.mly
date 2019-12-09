%{ open Ast %}

%token <string> EVENT
%token <string> VAR
%token <int> NUM
%token EMPTY CHOICE LPAR RPAR CONCAT OMEGA POWER PLUS MINUS TRUE FALSE DISJ CONJ SPACES ENTIL TESTPRO
%token EOF GT LT EQ 

%left POWER
%left CHOICE
%left CONCAT
%left DISJ
%left CONJ

%start prog ee
%type <Ast.entilment> ee
%type <Ast.program> prog

%%

ee: r = entailment EOF { r }


prog: r = TESTPRO {PROG []}

term:
| str = VAR { Var str }
| LPAR r = term RPAR { r }
| a = term PLUS b = NUM {Plus (a, b)}
| a = term MINUS b = NUM {Minus (a, b)}

pure:
| TRUE {TRUE}
| FALSE {FALSE}
| LPAR r = pure RPAR { r }
| a = term GT b = NUM {Gt (a, b)}
| a = term LT b = NUM {Lt (a, b)}
| a = term EQ b = NUM {Eq (a, b)}
| a = pure CONJ b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}


es:
| EMPTY { Emp }
| str = EVENT { Event str }
| LPAR r = es RPAR { r }
| a = es CHOICE b = es { ESOr(a, b) }
| r = es POWER t = term { Ttimes(r, t )}
| r = es POWER OMEGA { Omega r }
| a = es CONCAT b = es { Cons(a, b) } 

effect:
| LPAR r = effect RPAR { r }
| a = pure  CONJ  b= es  {Effect (a, b)}
| a = effect  DISJ  b=effect  {Disj (a,b)}

entailment:
| lhs = effect SPACES ENTIL SPACES rhs = effect {EE (lhs, rhs)}