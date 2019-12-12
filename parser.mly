%{ open Ast %}
%{ open List %}

%token <string> EVENT
%token <string> VAR
%token <int> INTE
%token <float> FLOATE
%token <string> STRING
%token <bool> TRUEE  
%token <bool> FALSEE
%token EMPTY EVENTKEY CHOICE LPAR RPAR CONCAT OMEGA POWER PLUS MINUS TRUE FALSE DISJ CONJ   ENTIL INTT FLOATT BOOLT VOIDT 
%token LBRACK RBRACK COMMA SIMI  IF ELSE REQUIRE ENSURE LSPEC RSPEC
%token EOF GT LT EQ  INCLUDE SHARP

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



type_: 
| INTT {INT}
| FLOATT {FLOAT}
| BOOLT {BOOL}
| VOIDT {VOID}

singleP: t = type_   var = VAR {[(t, var)]}

param:
| {[]}
| p = singleP {p}
| p1 = singleP  COMMA  p2 = param {append p1 p2 }

real_singleP: e = expres_help {[e]}

real_param:
| p = real_singleP {p}
| p1 = real_singleP  COMMA  p2 = real_param {append p1 p2 }

expres_help : 
| {Unit}
| t = INTE {Integer t}
| f = FLOATE {Float f}
| b =  TRUEE {Bool b }
| b =  FALSEE {Bool b }
| v = VAR {Variable v}
| t = type_ name = VAR EQ e = expres_help {LocalDel (t, name, e)}
| name = VAR LPAR vlist = real_param RPAR {Call (name, vlist)}
| v = VAR EQ e = expres_help{Assign (v, e)}
| EVENTKEY LPAR ev = STRING RPAR {EventRaise ev}


expres:
| e = expres_help {e } 
| e1 = expres_help SIMI e2 = expres {Seq (e1, e2)}
| IF LPAR e1 = expres_help RPAR LBRACK e2 = expres RBRACK ELSE LBRACK e3 = expres RBRACK {IfElse (e1, e2, e3)}


meth : t = type_   name = VAR   LPAR p = param RPAR s = spec LBRACK e = expres RBRACK {Method (Meth (t , name, p, s, e))}
head : SHARP INCLUDE str= STRING {Include str} 

prog1:
| me =  head { [me]}
| hd =meth   tl = prog {hd::tl}

prog:
| me =  meth { [me]}
| hd =head   tl = prog1 {hd::tl}

spec: LSPEC REQUIRE e1 = effect  ENSURE e2 = effect RSPEC {PrePost(e1, e2)}

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