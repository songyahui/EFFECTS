(*----------------------------------------------------
----------------------PRINTING------------------------
----------------------------------------------------*)

open String
open List
open Ast
open Printf
open Askz3
open Int32


exception Foo of string

let rec iter f = function
  | [] -> ()
  | [x] ->
      f true x
  | x :: tl ->
      f false x;
      iter f tl

let rec addConstrain effect addPi =
  match effect with
    Effect (pi, eff) -> Effect ( (PureAnd (pi, addPi)), eff)
  | Disj (effL1, effL2) -> Disj (addConstrain effL1 addPi, addConstrain effL2 addPi)
  ;;

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    bprintf buf "%s\n" (get_name x);
    let children = get_children x in
    iter (print_child indent) children
  and print_child indent is_last x =
    let line =
      if is_last then
        "└── "
      else
        "├── "
    in
    bprintf buf "%s%s" indent line;
    let extra_indent =
      if is_last then
        "    "
      else
        "│   "
    in
    print_root (indent ^ extra_indent) x
  in
  Buffer.add_string buf line_prefix;
  print_root line_prefix x

let printTree ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
  to_buffer ?line_prefix ~get_name ~get_children buf x;
  Buffer.contents buf

type binary_tree =
  | Node of string * (binary_tree  list )
  | Leaf

let get_name = function
    | Leaf -> "."
    | Node (name, li) -> name;;

let get_children = function
    | Leaf -> []
    | Node (_, li) -> List.filter ((<>) Leaf) li;;


(*All the entailmnet rules, but so far not been used*)
type rule = LHSOR   | RHSOR 
          | LHSEX   | RHSEX 
          | LHSSUB  | RHSSUB 
          | LHSCASE | RHSCASE 
          | UNFOLD  | DISPROVE 
          | FRAME   | REOCCUR

(*the effects entailment context*)
type context =  ( pure * es * pure * es) list

type hypotheses = (effect * effect) list

(*To pretty print terms*)
let rec showTerms (t:terms):string = 
  match t with
    Var name -> name
  | Number n -> string_of_int n
  | Plus (t1, t2) -> (showTerms t1) ^ ("+") ^ (showTerms t2)
  | Minus (t1, t2) -> (showTerms t1) ^ ("-") ^ (showTerms t2)

  ;;

(*To pretty print event sequences*)
let rec showES (es:es):string = 
  match es with
    Bot -> "_|_"
  | Emp -> "emp"
  | Event ev -> ev 
  | Cons (es1, es2) -> "("^(showES es1) ^ "." ^ (showES es2)^")"
  | ESOr (es1, es2) -> "("^(showES es1) ^ "|" ^ (showES es2)^")"
  | Ttimes (es, t) -> "("^(showES es) ^ "^" ^ (showTerms t)^")"
  | Omega es -> "("^(showES es) ^ "^" ^  "w" ^")"
  | Underline -> "_"
  | Kleene es -> "(" ^ (showES es) ^ "^" ^ "*"^")"
  | Range (esList) -> 
      let rec helperHere acc esL =
        match esL with
          [] -> acc ^ "}"
        | [x] -> acc ^ showES x ^"}"
        | x::xs ->  (helperHere (acc ^  showES x ^ ", ") xs)
      in
      let range = helperHere "{" esList in 
      range 
  ;;


let rec showESReg (es:es):string = 
  match es with
  | Bot -> "false"
  | Emp -> ""
  | Event ev -> ev 
  | Cons (es1, es2) -> (showESReg es1) ^ (showESReg es2)
  | ESOr (es1, es2) -> "("^(showESReg es1) ^ "|" ^ (showESReg es2)^")"
  | Kleene es -> "(" ^"(" ^ (showESReg es) ^")" ^ "*"^")"
  | _ -> raise (Foo "showESReg exception!")
  ;;

let encodeStrToInt (str:string):int = 
  match String.get str 0 with
    'A' -> 13
  | 'B' -> 7 
  | 'C' -> 1
  | 'D' -> 4
  | 'E' -> 5
  | 'F' -> 6
  | 'G' -> 7 
  | 'H' -> 8
  | 'I' -> 9
  | 'J' -> 10 
  | 'K' -> 11
  | 'L' -> 12
  | 'M' -> 13
  | 'N' -> 14
  | 'P' -> 15
  | 'Q' -> 16
  | 'R' -> 17
  | 'S' -> 18
  | 'T' -> 19
  | 'V' -> 20
  | 'X' -> 21
  | 'Y' -> 22
  | 'Z' -> 23
  | _ -> 24
  ;;

let rec termToInt (t:terms) : int = 
match t with
      Var s -> encodeStrToInt s
    | Number num -> num 
    | Plus (t1, t2) -> termToInt t1 + termToInt t2
    | Minus (t1, t2) -> termToInt t1 - termToInt t2
  
;;

let rec regToInt (esIn:es):int32  = 
  let rec helper es : int = 
    match es with
    | Bot -> -1
    | Emp -> 0
    | Underline -> 5
    | Event ev ->  encodeStrToInt ev
    | Cons (es1, es2) -> 5 * (helper es1) + (helper es2)
    | ESOr (es1, es2) -> (helper es1) + 7* (helper es2) 
    | Kleene es ->  (helper es) * 11
    | Ttimes (esIn, t) -> (helper esIn (*+ termToInt t*) ) * 13
    | Omega esIn -> (helper esIn ) * 17
    | Range (esList) -> 
        (let range = List.fold_left (fun acc a -> acc + (helper a)) 0 esList in 
        range 
        )
  in 
  let temp =  (helper esIn) in 
  
  of_int temp
  ;;



(*To pretty print pure formulea*)
let rec showPure (p:pure):string = 
  match p with
    TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | Gt (t1, t2) -> (showTerms t1) ^ ">" ^ (showTerms t2)
  | Lt (t1, t2) -> (showTerms t1) ^ "<" ^ (showTerms t2)
  | GtEq (t1, t2) -> (showTerms t1) ^ ">=" ^ (showTerms t2)
  | LtEq (t1, t2) -> (showTerms t1) ^ "<=" ^ (showTerms t2)
  | Eq (t1, t2) -> (showTerms t1) ^ "=" ^ (showTerms t2)
  | PureOr (p1, p2) -> "("^showPure p1 ^ "\\/" ^ showPure p2^")"
  | PureAnd (p1, p2) -> "("^showPure p1 ^ "/\\" ^ showPure p2^")"
  | Neg p -> "(~" ^ "(" ^ showPure p^"))"
  ;; 

(*To pretty print effects*)
let rec showEffect (e:effect) :string = 
  match e with
    Effect (p, es) -> 
      showPure p ^ "/\\" ^ showES es
  | Disj (es1, es2) -> "(" ^ showEffect es1 ^ ")\\/("  ^ showEffect es2^")"
  ;;

(*To pretty print effects entialments*)


let showEntailmentEff (eff1:effect)( eff2:effect):string = showEffect eff1 ^ " |- "  ^ showEffect eff2;;

(*To pretty print event sequence entailment*)
let showEntailmentES (es1:es) (es2:es):string = showES es1 ^ " |- "  ^ showES es2;;

let showEntailmentESReg (es1:es) (es2:es):string = showESReg es1 ^ " |- "  ^ showESReg es2;;

(*To pretty print entialment rules*)
let showRule (r:rule):string = 
  match r with
    LHSOR -> " [LHSOR] "
  | RHSOR -> " [RHSOR] "
  | LHSEX -> " [LHSEX] "  
  | RHSEX -> " [RHSEX] " 
  | LHSSUB -> " [LHSSUB] "
  | RHSSUB -> " [RHSSUB] "
  | LHSCASE -> " [LHSCASE] "
  | RHSCASE -> " [RHSCASE] "
  | UNFOLD  -> " [UNFOLD] "
  | DISPROVE -> " [DISPROVE] "
  | FRAME  -> " [FRAME] "
  | REOCCUR -> " [REOCCUR] "

(*To pretty print all the context entailments*)
let rec showContext (d:context):string = 
  match d with
    [] -> ""
  | (piL, esL, piR, esR)::rest -> (showEntailmentEff (Effect (piL, esL)) (Effect (piR, esR)) )^ ("\n") ^ showContext rest
  ;;

let rec reverseEs (es:es) : es = 
  match es with 
    Bot -> Bot
  | Emp -> Emp
  | Event ev -> Event ev 
  | Cons (es1, es2) -> Cons (reverseEs es2, reverseEs es1)
  | ESOr (es1, es2) -> ESOr (reverseEs es1, reverseEs es2)
  | Ttimes (es1, t) -> Ttimes (reverseEs es1, t)
  | Omega (es1) ->  Omega (reverseEs es1) 
  | Underline -> Underline
  | Kleene es1 ->  Kleene (reverseEs es1)
  | Range (esList) -> 
    (let range = List.map (fun a -> reverseEs a) esList in 
    Range range 
  )
  ;;

let rec reverseEff (eff:effect) : effect =
  match eff with 
    Effect (p,es) ->  Effect (p, reverseEs es)
  | Disj (eff1, eff2) -> Disj ((reverseEff eff1), (reverseEff eff2)) 
  ;;

let rec substituteTermWithAgr (t:terms) (realArg:expression) (formalArg: var):terms = 
  match t with 
    Var str -> if String.compare formalArg str == 0 then 
    (
      match realArg with 
        Integer n -> Number n
      | Variable v -> Var v
      | Bool true -> Number 1
      | Bool false -> Number 0
      | BinOp (Variable v, Integer n, "+") -> Plus (Var v, Number n)
      | BinOp (Variable v, Integer n, "-") -> Minus (Var v, Number n)
      | _ -> raise (Foo "substituteTermWithAgr exception")
    )
    else Var str 
  | Number n -> Number n
  | Plus (term, n) -> Plus (substituteTermWithAgr term realArg formalArg, n)
  | Minus (term, n) -> Minus (substituteTermWithAgr term realArg formalArg, n)
  ;;

let rec substituteESWithAgr (es:es) (realArg:expression) (formalArg: var):es = 
  match es with 
    Bot  -> es
  | Emp  -> es
  | Event ev  -> es
  | Cons (es1, es2) ->  Cons (substituteESWithAgr es1 realArg formalArg, substituteESWithAgr es2 realArg formalArg)
  | ESOr (es1, es2) ->  ESOr (substituteESWithAgr es1 realArg formalArg, substituteESWithAgr es2 realArg formalArg)
  | Ttimes (esIn, t) -> Ttimes (substituteESWithAgr esIn realArg formalArg, substituteTermWithAgr t realArg formalArg)
  | Kleene esIn -> Kleene (substituteESWithAgr esIn realArg formalArg)
  | Omega esIn -> Omega (substituteESWithAgr esIn realArg formalArg)
  | Underline -> es
  | Range (esList) -> 
    (let range = List.map (fun a -> substituteESWithAgr a realArg formalArg) esList in 
    Range range 
    )
  ;;


let rec splitDisj (p:pure) (es:es) :effect =
  match p with 
    PureOr (p1, p2) -> Disj (splitDisj p1 es, splitDisj p2 es) 
  | _ -> Effect (p, es) 
  ;;

let rec normalPureToDisj (p:pure):pure = 
  match p with 
    PureAnd (p1, PureOr(pIn1, pIn2)) ->  
      let dealP1 = normalPureToDisj p1 in
      let temp1 = normalPureToDisj (PureAnd(dealP1, pIn1)) in 
      let temp2 = normalPureToDisj (PureAnd(dealP1, pIn2)) in 
      PureOr (temp1 , temp2 )
  | PureAnd (PureOr(pIn1, pIn2), p2) ->  
      let dealP2 = normalPureToDisj p2 in
      let temp1 = normalPureToDisj (PureAnd(dealP2, pIn1)) in 
      let temp2 = normalPureToDisj (PureAnd(dealP2, pIn2)) in 
      PureOr (temp1 , temp2 )
  | Neg pi -> Neg (normalPureToDisj pi)
  | _ -> p
  ;;

let rec deletePureOrInEff (eff:effect):effect = 
  match eff with 
    Effect (pi, es) -> 
      let disjPure = normalPureToDisj pi in
      splitDisj disjPure es
  | Disj (eff1, eff2) -> Disj ((deletePureOrInEff eff1), (deletePureOrInEff eff2))
  ;;

let rec compareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> true
  | (Number n1, Number n2) -> n1 == n2 
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> compareTerm tIn1 tIn2 && compareTerm num1  num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> compareTerm tIn1 tIn2 && compareTerm num1  num2
  | _ -> false 
  ;;


let rec stricTcompareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> String.compare s1 s2 == 0
  | (Number n1, Number n2) -> n1 == n2 
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | _ -> false 
  ;;

let rec comparePure (pi1:pure) (pi2:pure):bool = 
  match (pi1 , pi2) with 
    (TRUE, TRUE) -> true
  | (FALSE, FALSE) -> true 
  | (Gt (t1, t11), Gt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Lt (t1, t11), Lt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (GtEq (t1, t11), GtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (LtEq (t1, t11), LtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Eq (t1, t11), Eq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (PureOr (p1, p2), PureOr (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (PureAnd (p1, p2), PureAnd (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Neg p1, Neg p2) -> comparePure p1 p2
  | _ -> false
  ;;

let rec getAllPi piIn acc= 
    (match piIn with 
      PureAnd (pi1, pi2) -> append (getAllPi pi1 acc ) (getAllPi pi2 acc )
    | _ -> append acc [piIn]
    )
    ;;

let rec existPi pi li = 
    (match li with 
      [] -> false 
    | x :: xs -> if comparePure pi x then true else existPi pi xs 
    )
    ;;

let rec normalTerms (t:terms):terms  = 
  match t with 
    Minus (Minus(s, Number n1), Number n2) ->  Minus(s, Number (n1 + n2))
  | Minus (Number n1, Number n2) ->  Number (n1- n2) 
  | Plus (Number n1, Number n2) -> Number (n1 + n2)
  | _ -> t 
  ;;



let rec aCompareES es1 es2 = 
  let rec subESsetOf (small : es list) (big : es list) :bool = 
    let rec oneOf a set :bool = 
      match set with 
        [] -> false 
      | y:: ys -> if aCompareES a y then true else oneOf a ys
    in 
    match small with 
      [] -> true 
    | x :: xs -> if oneOf x big == false then false else subESsetOf xs big
  in 

  match (es1, es2) with 
    (Bot, Bot) -> true
  | (Emp, Emp) -> true
  | (Event s1, Event s2) -> 
    String.compare s1 s2 == 0
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> 
    if (aCompareES es1L es2L) == false then false
    else (aCompareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      if ((aCompareES es1L es2L) && (aCompareES es1R es2R)) then true 
      else ((aCompareES es1L es2R) && (aCompareES es1R es2L))
  | (Kleene esL, Kleene esR) -> aCompareES esL esR
  | (Range (esList1), Range (esList2)) ->  subESsetOf esList1 esList2 && subESsetOf esList2 esList1
  | _ -> false
;;
 

let rec normalES es pi = 
  match es with
    Bot -> es
  | Emp -> es
  | Event ev -> es
  | Underline -> Underline
  | Cons (Cons (esIn1, esIn2), es2)-> normalES (Cons (esIn1, Cons (esIn2, es2))) pi
  | Cons (es1, es2) -> 
      let normalES1 = normalES es1 pi in
      let normalES2 = normalES es2 pi in
      (match (normalES1, normalES2) with 
        (Emp, _) -> normalES2
      | (_, Emp) -> normalES1
      | (Bot, _) -> Bot
      | (Omega _, _ ) -> normalES1

      | (Kleene (esIn1), Kleene (esIn2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2)
      | (Kleene (esIn1), Cons(Kleene (esIn2), es2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2) 

      | (normal_es1, normal_es2) -> 
        match (normal_es1, normal_es2) with 
        |  (Cons (esIn1, esIn2), es2)-> normalES (Cons (esIn1, Cons (esIn2, es2))) pi 
        |  (ESOr (or1, or2), es2) -> normalES (ESOr ( (Cons (or1, es2)),  (Cons (or2, es2)))) pi
        |  (es1, ESOr (or1, or2)) -> normalES (ESOr ( (Cons (es1, or1)),  (Cons (es1, or2)))) pi
        | _-> Cons (normal_es1, normal_es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES es1 pi, normalES es2 pi) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (ESOr(es1In, es2In), norml_es2 ) ->
        if aCompareES norml_es2 es1In || aCompareES norml_es2 es2In then ESOr(es1In, es2In)
        else ESOr (ESOr(es1In, es2In), norml_es2 )
      | (norml_es2, ESOr(es1In, es2In) ) ->
        if aCompareES norml_es2 es1In || aCompareES norml_es2 es2In then ESOr(es1In, es2In)
        else ESOr (norml_es2, ESOr(es1In, es2In))
      | (Emp, Kleene norml_es2) ->  Kleene norml_es2
      | (Kleene norml_es2, Emp) ->  Kleene norml_es2

      | (norml_es1, norml_es2) -> 
        if aCompareES  norml_es1 norml_es2 == true then norml_es1
        else 
        (match (norml_es1, norml_es2) with
          (norml_es1, Kleene norml_es2) ->  
          if aCompareES norml_es1 norml_es2 == true then Kleene norml_es2
          else ESOr (norml_es1, Kleene norml_es2)
        | (Kleene norml_es2, norml_es1) ->  
          if aCompareES norml_es1 norml_es2 == true then Kleene norml_es2
          else ESOr (Kleene norml_es2, norml_es1)
        |  _-> ESOr (norml_es1, norml_es2)
        )
      ;)
  (*
  | Cons (es1, es2) -> 
      let normalES1 = normalES es1 pi in
      let normalES2 = normalES es2 pi in
      (match (normalES1, normalES2) with 
        (Emp, _) -> normalES2
      | (_, Emp) -> normalES1
      | (Bot, _) -> Bot
      | (Omega _, _ ) -> normalES1
      | (normal_es1, normal_es2) -> Cons (normal_es1, normal_es2)
      ;)
      
  | ESOr (es1, es2) -> 
      (match (normalES es1 pi, normalES es2 pi) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (norml_es1, norml_es2) -> ESOr (norml_es1, norml_es2)
      ;)
      *)
  | Ttimes (es1, terms) -> 
      let t = normalTerms terms in 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ -> 
        let allPi = getAllPi pi [] in 
        if (existPi (Eq (terms, Number 0)) allPi) || (compareTerm t (Number 0 )) then Emp else Ttimes (normalInside, t))
        (*else if (existPi (Eq (terms, n)) allPi)) then Emp else Ttimes (normalInside, t))*)
  | Omega es1 -> 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ ->  Omega normalInside)
  | Kleene es1 -> 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | Kleene esIn1 ->  Kleene (normalES esIn1 pi)
      | ESOr(Emp, aa) -> Kleene aa
      | _ ->  Kleene normalInside)

  | Range (esList) -> 
      (let range = List.map (fun a -> normalES a pi) esList in 
      Range range 
      )
  ;;

let entailConstrains pi1 pi2 = 

  let sat = not (askZ3 (Neg (PureOr (Neg pi1, pi2)))) in
  (*
  print_string (showPure pi1 ^" -> " ^ showPure pi2 ^" == ");
  print_string (string_of_bool (sat) ^ "\n");
  *)
  sat;;

let rec normalPure (pi:pure):pure = 
  let allPi = getAllPi pi [] in
  let rec clear_Pi pi li = 
    (match li with 
      [] -> [pi]
    | x :: xs -> if existPi pi li then clear_Pi x xs else append [pi] (clear_Pi x xs)
    )in 
  let finalPi = clear_Pi TRUE allPi in
  let rec connectPi li acc = 
    (match li with 
      [] -> acc 
    | x :: xs -> if entailConstrains TRUE x then (connectPi xs acc) else PureAnd (x, (connectPi xs acc)) 
    ) in 
  let filte_true = List.filter (fun ele-> not (comparePure ele TRUE)  ) finalPi in 
  if length filte_true == 0 then  TRUE
  else connectPi (tl filte_true) (hd filte_true)
  ;;



