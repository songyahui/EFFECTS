%{ open Ast %}
%{ open List %}

%token <string> EVENT
%token <string> VAR
%token <int> INTE
%token <string> STRING
%token <bool> TRUEE  
%token <bool> FALSEE
%token EMPTY ASSERTKEY EVENTKEY CHOICE LPAR RPAR CONCAT OMEGA POWER PLUS MINUS TRUE FALSE DISJ CONJ   ENTIL INTT BOOLT VOIDT 
%token LBRACK RBRACK COMMA SIMI  IF ELSE REQUIRE ENSURE LSPEC RSPEC RETURN
%token EOF GT LT EQ GTEQ LTEQ INCLUDE SHARP EQEQ UNDERLINE KLEENE NEGATION
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND

%left CHOICE
%left CONCAT
%left DISJ
%left CONJ

%start prog ee es_p ltl_p
%type <(Ast.entilment) list > ee
%type <Ast.program> prog
%type <Ast.es> es_p
%type <(Ast.ltl) list > ltl_p
%%

ee: 
| EOF {[]}
| a = entailment SIMI r = ee { append [a] r }


es_p: r = es EOF { r }

ltl_p: 
| EOF {[]}
| a = ltl SIMI r = ltl_p { append [a] r }




type_: 
| INTT {INT}
| BOOLT {BOOL}
| VOIDT {VOID}

singleP: t = type_   var = VAR {[(t, var)]}

param:
| {[]}
| p = singleP {p}
| p1 = singleP  COMMA  p2 = param {append p1 p2 }

real_singleP: e = expres {[e]}

real_param:
| p = real_singleP {p}
| p1 = real_singleP  COMMA  p2 = real_param {append p1 p2 }

expres_help : 
| {Unit}
| RETURN {Return}
| t = INTE {Integer t}
| b =  TRUEE {Bool b }
| b =  FALSEE {Bool b }
| v = VAR {Variable v}
| s = STRING {String s}
| t = type_ name = VAR EQ e = expres_help {LocalDel (t, name, e)}
| name = VAR LPAR vlist = real_param RPAR {Call (name, vlist)}
| v = VAR EQ e = expres_help{Assign (v, e)}
| EVENTKEY LPAR ev = STRING p=parm RPAR {EventRaise (ev,p)}
| ASSERTKEY LPAR eff = effect RPAR {Assertion eff}

cond:
| e1 = expres_help  EQEQ e2 = expres_help {Cond (e1, e2 ,"==")}
| e1 = expres_help  LTEQ e2 = expres_help {Cond (e1, e2 ,"<=")}
| e1 = expres_help  GTEQ e2 = expres_help {Cond (e1, e2 ,">=")}
| e1 = expres_help  GT e2 = expres_help {Cond (e1, e2 ,">")}
| e1 = expres_help  LT e2 = expres_help {Cond (e1, e2 ,"<")}


expres:
| e = expres_help {e } 
| e1 = expres_help SIMI e2 = expres {Seq (e1, e2)}
| IF LPAR e1 = cond RPAR LBRACK e2 = expres RBRACK ELSE LBRACK e3 = expres RBRACK {IfElse (e1, e2, e3)}
| e1 = expres_help PLUS e2 = expres_help {BinOp(e1, e2,"+" )}
| e1 = expres_help MINUS e2 = expres_help {BinOp(e1, e2,"-" )}

meth : t = type_   name = VAR   LPAR p = param RPAR s = spec LBRACK e = expres RBRACK {Method (Meth (t , name, p, s, e))}
head : SHARP INCLUDE str= STRING {Include str} 


prog_rest:
| EOF {[]}
| tl = prog hd = prog_rest  {append tl hd}

prog:
| me =  meth p = prog_rest {append [me] p}
| hd =head  p = prog_rest {append [hd] p}

spec: LSPEC REQUIRE e1 = effect  ENSURE e2 = effect RSPEC {PrePost(e1, e2)}

term:
| str = VAR { Var str }
| n = INTE {Number n}
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, Number (0 -  b))}

| LPAR a = term MINUS b = term RPAR {Minus (a, b )}

| LPAR a = term PLUS b = term RPAR {Plus (a, b)}



pure:
| TRUE {TRUE}
| FALSE {FALSE}
| NEGATION LPAR a = pure RPAR {Neg a}
| LPAR r = pure RPAR { r }
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}
| a = pure CONJ b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}

es_range :
| single = es {[single]}
| e = es_range COMMA eddd = es {append e [eddd]}

parm:
| {None}
| LPAR i=INTE RPAR {Some i}


es:
| EMPTY { Emp }
| str = EVENT p=parm { Event (str, p) }
| LPAR r = es RPAR { r }
| a = es CHOICE b = es { ESOr(a, b) }
| a = es CONJ b = es { ESAnd(a, b) }
| LPAR r = es POWER t = term RPAR { Ttimes(r, t )}
| LPAR r = es POWER OMEGA RPAR{ Omega r }
| UNDERLINE {Underline}
| a = es CONCAT b = es { Cons(a, b) } 
| LPAR a = es POWER KLEENE RPAR{Kleene a}
| LBRACK a = es_range RBRACK {Range a}
| NEGATION LPAR a = es RPAR {Not a}




effect:
| LPAR r = effect RPAR { r }
| a = pure  CONJ  b= es  {Effect (a, b)}
| a = effect  DISJ  b=effect  {Disj (a,b)}

entailment:
| lhs = effect   ENTIL   rhs = effect {EE (lhs, rhs)}


ltl : 
| s = VAR {Lable s} 
| LPAR r = ltl RPAR { r }
| NEXT p = ltl  {Next p}
| LPAR p1= ltl UNTIL p2= ltl RPAR {Until (p1, p2)}
| GLOBAL p = ltl {Global p}
| FUTURE p = ltl {Future p}
| LTLNOT p = ltl {NotLTL p}
| LPAR p1= ltl IMPLY p2= ltl RPAR {Imply (p1, p2)}
| LPAR p1= ltl LILAND p2= ltl RPAR {AndLTL (p1, p2)}  

