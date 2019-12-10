%{ open Ast %}
%{ open List %}

%token <string> EVENT
%token <string> VAR
%token <int> INTE
%token <float> FLOATE
%token <string> STRING
%token EMPTY CHOICE LPAR RPAR CONCAT OMEGA POWER PLUS MINUS TRUE FALSE DISJ CONJ   ENTIL TESTPRO INTT FLOATT BOOLT VOIDT 
%token LBRACK RBRACK COMMA 
%token EOF GT LT EQ 

%left POWER
%left CHOICE
%left CONCAT
%left DISJ
%left CONJ

%start meth ee
%type <Ast.entilment> ee
%type <Ast.meth> meth

%%

ee: r = entailment EOF { r }


prog: TESTPRO {PROG []}

type_: 
| INTT {INT}
| FLOATT {FLOAT}
| BOOLT {BOOL}
| VOIDT {VOID}

singleP: t = type_   var = VAR {[(t, var)]}

param:
| {[]}
| p = singleP {p}
| p1 = singleP   COMMA   p2 = param {append p1 p2 }

expres:
| {Unit}
| t = INTE {Integer t}

meth: t = type_   name = VAR   LPAR p = param RPAR LBRACK e= expres RBRACK {Meth (t, name, p, PrePost(Effect(TRUE, Emp),Effect(TRUE, Emp)), e)}
 

term:
| str = VAR { Var str }
| LPAR r = term RPAR { r }
| a = term PLUS b = INTE {Plus (a, b)}
| a = term MINUS b = INTE {Minus (a, b)}

pure:
| TRUE {TRUE}
| FALSE {FALSE}
| LPAR r = pure RPAR { r }
| a = term GT b = INTE {Gt (a, b)}
| a = term LT b = INTE {Lt (a, b)}
| a = term EQ b = INTE {Eq (a, b)}
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
| lhs = effect   ENTIL   rhs = effect {EE (lhs, rhs)}