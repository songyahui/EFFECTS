open String
open List
open Ast
open Printf
open Parser
open Lexer
open Askz3
open Pretty



(*
ocamlc -o trs  Tree.ml  Rewriting.ml
*)

let rec compareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> true
  | (Number n1, Number n2) -> n1 == n2 
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | _ -> false 
  ;;



let rec stricTcompareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> String.compare s1 s2 == 0
  | (Number n1, Number n2) -> n1 == n2 
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && num1 == num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && num1 == num2
  | _ -> false 
  ;;

let rec comparePure (pi1:pure) (pi2:pure):bool = 
  match (pi1 , pi2) with 
    (TRUE, TRUE) -> true
  | (FALSE, FALSE) -> true 
  | (Gt (t1, n1), Gt (t2, n2)) -> stricTcompareTerm t1 t2 && n1 == n2
  | (Lt (t1, n1), Lt (t2, n2)) -> stricTcompareTerm t1 t2 && n1 == n2
  | (Eq (t1, n1), Eq (t2, n2)) -> stricTcompareTerm t1 t2 && n1 == n2
  | (PureOr (p1, p2), PureOr (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (PureAnd (p1, p2), PureAnd (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Neg p1, Neg p2) -> comparePure p1 p2
  | _ -> false
  ;;

(*----------------------------------------------------
------------------Utility Functions------------------
----------------------------------------------------*)
exception Foo of string


let rec nullable (pi :pure) (es:es) : bool=
  match es with
    Bot -> false 
  | Emp -> true
  | Event ev -> false 
  | Cons (es1 , es2) -> (nullable pi es1) && (nullable pi es2)
  | ESOr (es1 , es2) -> (nullable pi es1) || (nullable pi es2)
  | Ttimes (es1, t) -> askZ3 (PureAnd (pi, Eq (t,0))) 
  | Omega es1 -> false
  | Underline -> false
  | Kleene es1 -> true
;;
    
let rec fst (pi :pure) (es:es): event list = 
  match es with
    Bot -> []
  | Emp -> []
  | Event ev ->  [ev]
  | Omega es1 -> fst pi es1
  | Ttimes (es1, t) -> fst pi es1
  | Cons (es1 , es2) ->  if nullable pi es1 then append (fst pi es1) (fst pi es2) else fst pi es1
  | ESOr (es1, es2) -> append (fst pi es1) (fst pi es2)
  | Underline -> ["_"]
  | Kleene es1 -> fst pi es1
;;

let rec appendEff_ES eff es = 
  match eff with 
    Effect (p , es_eff) ->  Effect(p, Cons (es_eff, es))
  | Disj (eff1 , eff2)  ->  Disj (appendEff_ES eff1 es, appendEff_ES eff2 es)
  
  (*raise ( Foo "appendEff_ES exception!")*)
  ;;


let rec derivative (p :pure) (es:es) (ev:string): effect =
  match es with
    Bot -> Effect (FALSE,  Bot)
  | Emp -> Effect (FALSE,  Bot)
  | Underline -> Effect (p, Emp)
  | Event ev1 -> 
      if (String.compare ev "_") == 0 then  Effect (p, Emp)
      else if (String.compare ev1 ev) == 0 then Effect (p, Emp) else Effect (FALSE, Bot)
  | Omega es1 -> appendEff_ES (derivative p es1 ev) es
  | ESOr (es1 , es2) -> Disj (derivative p es1 ev, derivative p es2 ev)
  | Ttimes (es1, t) -> 
      let pi = PureAnd (Gt (t, 0), p) in
      let efF = derivative pi es1 ev in 
      let esT_minus1 = Ttimes (es1,  Minus (t, 1)) in
      appendEff_ES efF esT_minus1
  | Cons (es1 , es2) -> 
      if nullable p es1 
      then let efF = derivative p es1 ev in 
          let effL = appendEff_ES efF es2 in 
          let effR = derivative p es2 ev in 
          Disj (effL, effR)
      else let efF = derivative p es1 ev in 
          appendEff_ES efF es2    
          
  | Kleene es1 -> appendEff_ES  (derivative p es1 ev) es

;;


(*----------------------------------------------------
----------------------CONTAINMENT--------------------
----------------------------------------------------*)



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
    Minus (Number n1, n2) ->  Number (n1- n2) 
  | Plus (Number n1, n2) -> Number (n1 + n2)
  | _ -> t 
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
      | (normal_es1, normal_es2) -> Cons (normal_es1, normal_es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES es1 pi, normalES es2 pi) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (norml_es1, norml_es2) -> ESOr (norml_es1, norml_es2)
      ;)
  | Ttimes (es1, terms) -> 
      let t = normalTerms terms in 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ -> 
        let allPi = getAllPi pi [] in 
        if (existPi (Eq (terms, 0)) allPi) || (compareTerm t (Number 0 )) then Emp else Ttimes (normalInside, t))
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
      | _ ->  Kleene normalInside)
  ;;

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
    | x :: xs -> PureAnd (x, (connectPi xs acc)) 
    ) in 
  let filte_true = List.filter (fun ele-> not (comparePure ele TRUE)  ) finalPi in 
  if length filte_true == 0 then  TRUE
  else connectPi (tl filte_true) (hd filte_true)
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

let rec splitDisj (p:pure) (es:es) :effect =
  match p with 
    PureOr (p1, p2) -> Disj (splitDisj p1 es, splitDisj p2 es) 
  | _ -> Effect (p, es) 
  ;;

let rec deletePureOrInEff (eff:effect):effect = 
  match eff with 
    Effect (pi, es) -> 
      let disjPure = normalPureToDisj pi in
      splitDisj disjPure es
  | Disj (eff1, eff2) -> Disj ((deletePureOrInEff eff1), (deletePureOrInEff eff2))
  ;;


let rec normalEffect eff =
  let noPureOr  = deletePureOrInEff eff in 
  match noPureOr with
    Effect (p, es) -> 
      if (askZ3 p) == false then Effect (FALSE,  Bot)
      else if normalES es p== Bot then Effect (FALSE,  Bot)
      else Effect (normalPure p , normalES es p)
  | Disj (eff1, eff2) -> 
      match (normalEffect eff1, normalEffect eff2) with
        (Effect (_,  Bot), _) -> normalEffect eff2
      | (_, Effect (_,  Bot)) -> normalEffect eff1
      | _ -> Disj (normalEffect eff1, normalEffect eff2)
  ;;


let rec compareES es1 es2 = 
  match (es1, es2) with 
    (Bot, Bot) -> true
  | (Emp, Emp) -> true
  | (Event s1, Event s2) -> 
    String.compare s1 s2 == 0
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> (compareES es1L es2L) && (compareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      let one = ((compareES es1L es2L) && (compareES es1R es2R)) in
      let two =  ((compareES es1L es2R) && (compareES es1R es2L)) in 
      one || two
  | (Omega esL, Omega esR) ->compareES esL esR
  | (Ttimes (esL, termL), Ttimes (esR, termR)) -> 
      let insideEq = (compareES esL esR) in
      let termEq = compareTerm termL termR in
      insideEq && termEq
  | (Kleene esL, Kleene esR) -> compareES esL esR
  | (Underline, Underline ) -> true
  | _ -> false
;;

let rec compareEff eff1 eff2 =
  match (eff1, eff2) with
    (Effect (pi1, es1), Effect (pi2, es2)) -> compareES es1 es2
  | (Disj (eff11, eff12), Disj (eff21, eff22)) -> 
      let one =  (compareEff eff11  eff21) && (compareEff eff12  eff22) in
      let two =  (compareEff eff11  eff22) && (compareEff eff12  eff21 ) in
      one || two
  | _ -> false
  ;;

  

let rec reoccur piL esL piR esR delta = 
  match delta with 
  | [] -> false
  | (pi1, es1, pi2, es2) :: rest -> 
      if (compareEff (Effect(piL, esL)) (Effect(pi1, es1)) && compareEff (Effect(piR, esR))  (Effect(pi2, es2))) 
      then true

      else reoccur piL esL piR esR rest (*REOCCUR*) 
  ;;



let entailConstrains pi1 pi2 = 
  (*print_string (showPure pi1 ^" and " ^ showPure pi2 ^" ==> ");
  print_string (string_of_bool (askZ3 (PureAnd (pi1, pi2))) ^ "\n");*)
  let sat = askZ3 (Neg (PureOr (Neg pi1, pi2))) in
  if sat then false
  else true;;

let rec getPureFromEffect effect = 
  match effect with
    Effect (pi, _) -> pi
  | Disj (eff1, eff2) -> PureOr ((getPureFromEffect eff1), (getPureFromEffect eff2))
  ;;

let rec getAllVarFromES es = 
  match es with
  | Ttimes (_, Var s) -> [s]
  | Ttimes (_, Plus (Var s, _ )) -> [s]
  | Ttimes (_, Minus (Var s, _ )) -> [s]
  | Cons (es1, es2) -> append (getAllVarFromES es1 ) (getAllVarFromES es2 ) 
  | ESOr (es1, es2) -> append (getAllVarFromES es1 ) (getAllVarFromES es2 ) 
  | Omega (esIn) -> getAllVarFromES esIn
  | Kleene (esIn) -> getAllVarFromES esIn
  | _ -> []
  ;;

let rec getAllVarFromEff (eff:effect): string list = 
  match eff with 
    Effect (pi, es) -> getAllVarFromES es
  | Disj(eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
(*match effect with 
    Effect (pi, es) -> getAllVarFromES es
  | Disj (eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
*)
;;



(*used to generate the free veriables, for subsititution*)
let freeVar = ["t1"; "t2"; "t3"; "t4";"t5";"t6";"t7";"t8";"t9";"t10"
              ;"t11"; "t12"; "t13"; "t14";"t15";"t16";"t17";"t18";"t19";"t20"
              ;"t21"; "t22"; "t23"; "t24";"t25";"t26";"t27";"t28";"t29";"t30"];;



let rec getAfreeVar (varList:string list):string  =
  let rec findOne li = 
    match li with 
        [] -> raise ( Foo "freeVar list too small exception!")
      | x :: xs -> if (exist varList x) == true then findOne xs else x
  in
  findOne freeVar
;;

let rec pattermMatchingTerms terms pattern termNew:terms= 
  if (stricTcompareTerm terms pattern) ==  true then termNew 
  else match terms with 
        Plus (tp, num) -> Plus (pattermMatchingTerms tp pattern termNew, num)
      | Minus (tp, num) -> Minus (pattermMatchingTerms tp pattern termNew, num)
      | _ -> terms
  ;;

let rec substituteES es termOrigin termNew = 
  match es with 
  | Ttimes (es1, term) -> Ttimes (es1,  pattermMatchingTerms term termOrigin termNew)
  | Cons (es1, es2) -> Cons (substituteES es1 termOrigin termNew ,substituteES es2 termOrigin termNew ) 
  | ESOr (es1, es2) -> Cons (substituteES es1 termOrigin termNew ,substituteES es2 termOrigin termNew ) 
  | Omega (es1) -> Omega (substituteES es1 termOrigin termNew)
  | Kleene (es1) -> Kleene (substituteES es1 termOrigin termNew)
  | _ -> es
  ;;

let rec substituteEff (effect:effect) (termOrigin:terms) (termNew:terms) = 
  match effect with 
    Effect (pi, es) -> Effect (pi, substituteES es termOrigin termNew) 
  | Disj (eff1, eff2) -> Disj (substituteEff eff1 termOrigin termNew , substituteEff eff2 termOrigin termNew ) 
  ;;



let rec substituteESStar es termOrigin = 
  match es with 
  | Ttimes (es1, term) -> if (stricTcompareTerm term termOrigin) ==  true then Kleene (es1) else es
  
  (*Ttimes (es1,  pattermMatchingTermsStar term termOrigin )*)
  | Cons (es1, es2) -> Cons (substituteESStar es1 termOrigin  ,substituteESStar es2 termOrigin ) 
  | ESOr (es1, es2) -> Cons (substituteESStar es1 termOrigin  ,substituteESStar es2 termOrigin ) 
  | Omega (es1) -> Omega (substituteESStar es1 termOrigin )
  | Kleene (es1) -> Kleene (substituteESStar es1 termOrigin)
  | _ -> es
  ;;

let rec substituteEffStar (effect:effect) (termOrigin:terms) = 
  match effect with 
    Effect (pi, es) -> Effect (pi, substituteESStar es termOrigin) 
  | Disj (eff1, eff2) -> Disj (substituteEffStar eff1 termOrigin  , substituteEffStar eff2 termOrigin ) 
  ;;

let isEmp effect = 
  match effect with
    Effect (_ , Emp) -> true
  | _ -> false 

let isBot effect = 
  match effect with
    Effect (_ , Bot) -> true
  | _ -> false 

let getFst (a,b) = a ;;
let getSnd (a,b) = b ;;


let rec enForcePure eff1 eff2 = 
  match eff1 with 
    Effect (pi1, es1) ->
      (match eff2 with 
        Effect (pi2, es2) -> Effect(PureAnd (pi1, pi2), es2)
      | Disj (eff_1, eff_2) -> Disj (enForcePure eff1 eff_1, enForcePure eff1 eff_2)
      ) 
  | Disj (_,_) -> raise (Foo "enForcePure exception")
  ;;

let rec quantified_by_Term (term:terms) str = 
  match term with 
    Var s1 -> if String.compare s1 str == 0 then true else false
  | Plus (tIn1, num1) -> quantified_by_Term tIn1 str
  | Minus (tIn1, num1) -> quantified_by_Term tIn1 str
  | Number n -> raise (Foo "quantified_by_Term exception")
   ;;


let rec quantified_in_LHS esL str = 
  match esL with
  | Ttimes (es1, term) -> quantified_by_Term term str
  | Cons (es1, es2) -> quantified_in_LHS es1 str || quantified_in_LHS es2 str
  | Omega (es1) -> quantified_in_LHS es1 str
  | Kleene (es1) -> quantified_in_LHS es1 str
  | ESOr (es1, es2) -> raise (Foo "quantified_in_LHS exception")
  | _ -> false
  ;;

let rec getFirstVar (es :es): string option = 
    match es with 
      Cons (es1, es2) -> 
        (
        match getFirstVar es1 with 
          None -> getFirstVar es2 
        | Some str -> Some str
        )
    | ESOr (es1, es2) -> 
        (
        match getFirstVar es1 with 
          None -> getFirstVar es2 
        | Some str -> Some str
        )
    | Ttimes (esIn, t) -> 
        let rec getVarFromTerm term = 
          match term with 
            Var str -> Some str 
          | Number n -> None 
          | Plus (tt, n) -> getVarFromTerm tt 
          | Minus (tt, n) -> getVarFromTerm tt 
        in getVarFromTerm t 
    | Kleene esIn -> getFirstVar esIn
    | Omega esIn -> getFirstVar esIn
    | _ -> None
;;

let existialRHS piL esL esR varList :bool = 
  let rec checkExistTerm t s = 
    match t with 
      Var str -> String.compare s str == 0
    | Number n -> false 
    | Plus (tt, n) -> checkExistTerm tt s
    | Minus (tt, n) -> checkExistTerm tt s
  in 
  let rec checkEXISTinPure (p:pure) (str:string ) :bool = 
    match p with 
      Gt (t, n) -> checkExistTerm t str
    | Lt (t, n) -> checkExistTerm t str
    | Eq (t, n) -> checkExistTerm t str
    | PureOr (p1, p2) -> checkEXISTinPure p1  str || checkEXISTinPure p2  str
    | PureAnd (p1, p2) -> checkEXISTinPure p1  str || checkEXISTinPure p2  str
    | Neg pi -> checkEXISTinPure pi  str
    | _ -> false 
  in 
  
  let rec checkExist es str:bool =
    match es with 
      Cons (es1, es2) -> checkExist es1 str || checkExist es2 str
    | ESOr (es1, es2) -> checkExist es1 str || checkExist es2 str
    | Ttimes (esIn, te) -> 
      checkExistTerm te str
    | Kleene esIn -> checkExist esIn str
    | Omega esIn ->  checkExist esIn str
    | _ -> false
  in 
  match getFirstVar esR with 
    None -> false
  | Some (str) -> 
  (*print_string (str^"\n");*)
  (not (checkExist esL str || checkEXISTinPure piL str)) && not (List.mem str varList)
  ;;

let getInstansVal esL: int list = 
  let rec getAllVal (es :es): int list = 
    match es with 
      Cons (es1, es2) -> append (getAllVal es1) (getAllVal es2)
    | ESOr (es1, es2) -> append (getAllVal es1) (getAllVal es2)
    | Ttimes (esIn, t) -> 
        let rec getValFromTerm term = 
          match term with 
            Var str -> []
          | Number n -> [n] 
          | Plus (tt, n) -> getValFromTerm tt 
          | Minus (tt, n) -> getValFromTerm tt 
        in getValFromTerm t 
    | Kleene esIn -> getAllVal esIn
    | Omega esIn -> getAllVal esIn
    | _ -> []
  in 
  getAllVal esL
  ;;


let rec substituteTermWithVal (t:terms) (var1:string) (val1: int):terms = 
  match t with 
    Var str -> if String.compare var1 str == 0 then (Number val1) else Var str 
  | Number n -> Number n
  | Plus (term, n) -> Plus (substituteTermWithVal term var1 val1, n)
  | Minus (term, n) -> Minus (substituteTermWithVal term var1 val1, n)
  ;;

let rec substituteESWithVal (es:es) (var1:string) (val1: int):es = 
  match es with 
    Bot  -> es
  | Emp  -> es
  | Event ev  -> es
  | Cons (es1, es2) ->  Cons (substituteESWithVal es1 var1 val1, substituteESWithVal es2 var1 val1)
  | ESOr (es1, es2) ->  ESOr (substituteESWithVal es1 var1 val1, substituteESWithVal es2 var1 val1)
  | Ttimes (esIn, t) -> Ttimes (substituteESWithVal esIn var1 val1, substituteTermWithVal t var1 val1)
  | Kleene esIn -> Kleene (substituteESWithVal esIn var1 val1)
  | Omega esIn -> Omega (substituteESWithVal esIn var1 val1)
  | Underline -> es
  ;;

let instantiateEff (pi:pure) (es:es) (instances: int list): effect list = 
  match getFirstVar es with 
    None -> []
  | Some (str) ->  
    map (fun n -> Effect (pi, substituteESWithVal es str n) ) instances 
  ;;


(*-------------------------------------------------------------
--------------------Main Entrance------------------------------
---------------------------------------------------------------
This decision procedure returns a derivation tree and a boolean
value indicating the validility of the effect entailment
-------------------------------------------------------------*)

let rec containment (effL:effect) (effR:effect) (delta:context) (varList:string list): (binary_tree * bool * int) = 
  let normalFormL = normalEffect effL in 
  let normalFormR = normalEffect effR in
  let showEntail  = (*showEntailmentEff effL effR ^ " ->>>> " ^*)showEntailmentEff normalFormL normalFormR in 
  (*print_string(showEntail ^"\n");*)
  let unfoldSingle ev piL esL piR esR del = 
    let derivL = derivative piL esL ev in
    let derivR = derivative piR esR ev in
    let deltaNew = append del [(piL, esL, piR, esR)] in
    let (tree, result, states) = containment derivL derivR deltaNew varList in
    (Node (showEntailmentEff ( (Effect(piL, esL))) ((Effect(piR, esR))) ^ "   [Unfold with Fst = "^  ev ^ "]",[tree] ), result, states+1)
  in
  (*Unfold function which calls unfoldSingle*)
  let unfold del piL esL piR esR= 
    let rec remove_dups lst= 
      (match lst with
      | [] -> []
      | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
      )
      in
    let fstL = remove_dups (fst piL esL )in 

    let rec chceckResultAND li acc staacc:(bool *binary_tree list* int )=
      (match li with 
        [] -> (true, acc, staacc) 
      | ev::fs -> 
          let (tree, re, states) = unfoldSingle ev piL esL piR esR del in 
          if re == false then (false , tree::acc, staacc+states)
          else chceckResultAND fs (tree::acc) (staacc+states)
      )
    in 
    let (resultFinal, trees, states) = chceckResultAND fstL [] 0 in 
    (Node (showEntailmentEff ( (Effect(piL, esL))) ((Effect(piR, esR))) ,trees ), resultFinal, states)    
  
  in 
  match (normalFormL, normalFormR) with
    (Disj (effL1, effL2), _) -> 
    (*[LHSOR]*)
      let (tree1, re1, states1 ) = (containment effL1 effR delta varList) in
      if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree1] ),  false, states1)
      else 
        let (tree2, re2 , states2) = (containment effL2 effR delta varList) in
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree1; tree2] ), re2, states1+states2+1)
  | (_, Disj (effR1, effR2)) -> 
    (*[RHSOR]*)
      let (tree1, re1, states1 ) = (containment effL effR1 delta varList) in
      if re1 == true then (Node (showEntailmentEff normalFormL normalFormR ^ showRule RHSOR, [tree1] ), true, states1)
      else 
        let (tree2, re2 , states2) = (containment effL effR2 delta varList) in
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule RHSOR, [tree1; tree2] ), re2, states1+states2)
  | (Effect (piL, esL), Effect (piR, esR))-> 
      if entailConstrains piL piR == false then (Node(showEntail ^ "   [Contradictory]", []), false, 0)  
      else 
      (*Existential*)
        if existialRHS piL esL esR varList == true then
          let instanceFromLeft = getInstansVal esL in 
          (*print_string (List.fold_left (fun acc a  -> acc ^ string_of_int a ^ "\n") ""  instanceFromLeft );*)
          let instantiateRHS = instantiateEff piR esR instanceFromLeft in 

          let rec chceckResultOR li acc staacc=
            (match li with 
              [] -> (false , acc, staacc) 
            | rhs::rhss -> 
                let (tree, re, states) = containment (Effect (piL, esL)) rhs delta varList in 
                if re == true then (true , tree::acc, staacc+states)
                else chceckResultOR rhss (tree::acc) (staacc+states)
            )
          in 
          let (resultFinal, trees, states) = chceckResultOR instantiateRHS [] 0 in
          (Node(showEntail ^ "   [EXISTENTIAL]", trees ), resultFinal, states) 


      (*[DISPROVE]*)
        else if (comparePure piR FALSE == true ) then (Node(showEntail ^ "   [DISPROVE] "  , []), false, 0)
      (*[REFUTATION]*)
        else if (nullable piL esL) == true && (nullable piR esR) == false 
        then (Node(showEntail ^ "   [REFUTATION] "  , []), false, 0) 
      (*[Frame]*)
        else if (isEmp normalFormR) == true  
        then  (Node(showEntail^"   [Frame-Prove]" ^" with R = "^(showES esL ) , []),true, 0) 
      (*[Reoccur]*)
        else if (reoccur piL esL piR esR delta) == true  
        then (Node(showEntail ^ "   [Reoccur-Prove] "  , []), true, 0) 
      (*Unfold*)                    
      else 
        (match esL with
        (*LHSEX*)
        | Kleene esIn ->  
          unfold delta piL esL piR esR
        | Cons (Kleene esIn, _) -> 
          unfold delta piL esL piR esR
        | Ttimes (esIn, term) -> 
            (match term with 
              Var s -> 
                (match  entailConstrains piL (Eq (Var s, 0) ) with 
                  true -> (*[CASE SPLIT]*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain (Effect(piL, Emp)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSCASE ^ " *Pruning search*",[tree1] ), re1, states1)
                            else
                            let (tree2, re2 , states2) = (containment leftNonZero rightNonZero delta varList) in
                            (Node (showEntailmentEff normalFormL normalFormR ,[tree1; tree2] ), re1 && re2, states1+states2)
                | false -> (*[UNFOLD]*)unfold delta piL esL piR esR
                )
            | Plus  (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num))  (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
            | Minus (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
            | Number n -> unfold delta piL esL piR esR
            | _ -> print_endline (showEntailmentEff normalFormL normalFormR);
              raise ( Foo "term is too complicated exception1!")
            )
        | Cons (Ttimes (esIn, term), restES) -> 
            (match term with 
              Var s -> 
                (match  entailConstrains piL (Eq (Var s, 0) ) with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain (Effect(piL, restES)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1 , states1) = (containment leftZero rightZero delta varList) in
                            if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSCASE ^ " *Pruning search*",[tree1] ), re1, states1)
                            else 
                            let (tree2, re2, states2 ) = (containment leftNonZero rightNonZero delta varList) in
                            (Node (showEntailmentEff normalFormL normalFormR ,[tree1; tree2] ), re1 && re2, states1+states2)
                        | false -> (*UNFOLD*) unfold delta piL esL piR esR
                        )
              | Plus  (Var t, num) -> 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
              | Minus (Var t, num) -> 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
              | Number n -> unfold delta piL esL piR esR
              | _ -> print_endline (showEntailmentEff normalFormL normalFormR);
              raise ( Foo "term is too complicated exception2!")
            )
          | _ -> (*RHSEX*)
            (match esR with
              Ttimes (esInR, termR) -> 
                (match termR with 
                  Var s -> 
                        if quantified_in_LHS esL s then unfold delta piL esL piR esR
                        else 
                        (match  entailConstrains piR (Eq (Var s, 0) ) with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain normalFormL zeroCase in
                            let rightZero = addConstrain (Effect(piR, Emp)) zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == true then (Node (showEntailmentEff effL effR ,[tree1] ), true, states1) 
                            else 
                              let (tree2, re2, states2 ) = (containment leftNonZero rightNonZero delta varList) in
                              (Node (showEntailmentEff effL effR ,[tree1; tree2] ), re2, states1+states2)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
                | Minus (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
                | Number n -> unfold delta piL esL piR esR
                | _ -> print_endline (showEntailmentEff normalFormL normalFormR);
                raise ( Foo "term is too complicated exception3!")
                )
            | Cons (Ttimes (esInR, termR), restESR) -> 
                (match termR with 
                  Var s -> 
                        if quantified_in_LHS esL s then unfold delta piL esL piR esR
                        else 
                        (match  entailConstrains piL (Eq (Var s, 0) ) with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piR, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piR, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain normalFormL zeroCase in
                            let rightZero = addConstrain (Effect(piR, restESR)) zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == true then (Node (showEntailmentEff normalFormL normalFormR , [tree1] ), true, states1)
                            else 
                            let (tree2, re2, states2 ) =  (containment leftNonZero rightNonZero delta varList) in 
                            (Node (showEntailmentEff normalFormL normalFormR , [tree1; tree2] ), re2, states1+states2)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar)  in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
                | Minus (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar)  in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar)  in
                        let cons = PureOr (Gt (Var newVar, 0) , Eq (Var newVar, 0) ) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
                | Number n -> unfold delta piL esL piR esR
                | _ -> print_endline (showEntailmentEff normalFormL normalFormR);
                raise ( Foo "term is too complicated exception4!")
                )
            | _ -> (*UNFOLD*)unfold delta piL esL piR esR
            )
        )        
  ;;
  
(*----------------------------------------------------
----------------------TESTING-------------------------
----------------------------------------------------*)

type expectation = bool

type entailment =  (effect * effect * expectation) 




let ttest = (Plus ((Var "song"),1));;
let ttest1 = (Var "t");;
let estest = ESOr (Cons (Ttimes ((Event "a"), Var "t"),  (Event "a")), Cons ((Event "a"),(Event "b")));;
let puretest =  Eq (ttest1, 0);;
let testes = Effect (puretest, estest);; 
let testcontext =  [testes; testes];;
let testD = derivative puretest estest "a";;
let leftEff = Effect (TRUE, ESOr (Omega (Event "a"), Omega (Event "b"))) ;;
let rightEff = Effect (TRUE, Omega (Event "b")) ;;
let leftEff1 = Effect (TRUE, Cons (Event "a", Cons (Event "b", Event "c"))) ;;
let rightEff2 = Effect (TRUE, Cons (Event "a", Cons (Event "d", Event "c"))) ;;
let lhsss = Effect (TRUE, Cons (Ttimes ((Event "a"), Var "t"), Event "c"));;
let rhsss = Effect (TRUE, Omega ((Event "a")));;




(*Printf.printf "%s" (showTerms  ttest);;
Printf.printf "%s" (showES estest);;

Printf.printf "%s" (showPure puretest);;

Printf.printf "%s" (showEffect testes);;
Printf.printf "%s" (showContext testcontext );;*)

let a = Event "Tick" ;;
let b = Event "b" ;;
let c = Event "c" ;;
let ab = Cons (a,b) ;;
let bc = Cons (b,c) ;;
let aOrb = ESOr (a, b) ;;
let aOrc = ESOr (a, c) ;;
let ab_or_c = ESOr (ab, c) ;;
let omegaA = Omega (a);;
let omegaB = Omega (b);;
let omegaaOrb = Omega (aOrb);;

let createT es = Ttimes (es, Var "t" );;

let createS es = Ttimes (es, Var "s" );;

let createT_1 es = Ttimes (es, Minus (Var "t", 1) );;

let createS_1 es = Ttimes (es, Minus (Var "s", 1) );;


let printReport lhs rhs:string =
  let varList = append (getAllVarFromEff lhs) (getAllVarFromEff rhs) in  
  let (tree, re, states) = containment  lhs rhs [] varList in
  let result = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  let states = "[Explored "^ string_of_int (states+1) ^ " States]\n" in 
  let buffur = ( "===================================="^"\n" ^(showEntailmentEff lhs rhs)^"\n[Result] " ^(if re then "Succeed\n" else "Fail\n") ^ states ^"\n\n"^ result)
  in buffur
  ;;

let testcases : entailment list= 
  [

  (Effect(Gt (Var "t", 0), Cons (createT (Event "a"),omegaA))
  ,Effect(Gt (Var "t", 0), Cons (createT (Event "a"),omegaB))
  ,true)
  ;
  (Effect(TRUE, Cons (Cons (Event "a",createT_1 (Event "a")),omegaA))
  ,Effect(TRUE, Cons (createT (Event "a"),omegaB))
  ,true)
  ;
  (Effect(TRUE, Cons (Event "b", Ttimes (Cons (Event "a", Event "b"),Var "t")))
  ,Effect(TRUE, Cons (Ttimes (Cons (Event "a", Event "b"),Var "t"), Event "b"))
  ,true)
  ;
  (Effect(Gt (Var "t", 0), Cons (Event "b", Ttimes (Cons (Event "a", Event "b"),Var "t")))
  ,Effect(Gt (Var "t", 0), Cons (Ttimes (Cons (Event "a", Event "b"),Var "t"), Event "b"))
  ,true)
  ;
  (Effect(TRUE, Event "a")
  ,Effect(TRUE, Event "a")
  ,true)
  ;
  (Effect(TRUE, ab)
  ,Effect(TRUE, bc)
  ,true)
  ;
  (Effect(TRUE, a)
  ,Effect(TRUE, aOrb)
  ,true
  )
  ;
  (Effect(TRUE, aOrb)
  ,Effect(TRUE, a)
  ,true
  )
  ;
  (Effect(TRUE, ab)
  ,Effect(TRUE, a)
  ,true
  )
  ;
  (Effect(TRUE, omegaA)
  ,Effect(TRUE, omegaaOrb)
  ,true
  )
  ;
  (Effect(TRUE, omegaaOrb) 
  ,Effect(TRUE, omegaA) 
  ,true
  )
  ;
  (Effect(TRUE, createT a) 
  ,Effect(TRUE, createT a)
  ,true
  )
  ;
  (Effect(TRUE, createT a) 
  ,Effect(Gt(Var "t", 0), createT a)
  ,true
  )
  ;
  (Effect(TRUE, createT a)
  ,Effect(TRUE, createT ab)
  ,true
  )
  ;
  (Effect(TRUE, createT_1 a)
  ,Effect(TRUE, createT_1 a)
  ,true
  )
  ;
  (Effect(TRUE, Cons (Event "a" ,createT_1 a))
  ,Effect(TRUE, createT a)
  ,true
  )
  ;
  (Effect(TRUE, createT a)
  ,Effect(TRUE, Cons (Event "a" ,createT_1 a))
  ,true
  )
  ;
  (Effect(Gt(Var "t", -1), createT a)
  ,Effect(TRUE, Cons (Event "a" ,createT_1 a))
  ,true
  )
  ;
  (Effect(Gt(Var "t", 0), createT a)
  ,Effect(TRUE, createT_1 a)
  ,true
  )
  ;
   (*THIS ONE IS WRONG!*)
  (Effect(Gt(Var "s", 0), Cons (createT a ,createS b))
  ,Effect(TRUE, Cons (createT a ,createS_1 b))
  ,true
  )
  ;
  (Effect(TRUE, omegaA)
  ,Effect(TRUE, createT_1 a)
  ,true
  )
  ;
  (Effect(TRUE, Cons (Event "Tick" ,createT_1 a))
  ,Effect(TRUE, createT a)
  ,true
  )
  ;
  (Effect(TRUE, Cons (Event "a" ,createT_1 a)) 
  ,Effect(TRUE, createT a)
  ,true
  )
  
  ];;

let rec runTestcases (suites :entailment list) =
  match suites with
  [] -> ""
  | (lhs, rhs, expect) :: xs ->  
    print_string (printReport lhs rhs);
    runTestcases xs
    ;;

