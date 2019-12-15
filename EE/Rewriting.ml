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
  | Event ev1 -> 
      if (String.compare ev1 ev) == 0 then Effect (p, Emp) else Effect (FALSE, Bot)
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
;;


(*----------------------------------------------------
----------------------CONTAINMENT--------------------
----------------------------------------------------*)

let rec compareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> true
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | _ -> false 
  ;;



let rec stricTcompareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
    (Var s1, Var s2) -> String.compare s1 s2 == 0
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

let rec normalES es pi = 
  match es with
    Bot -> es
  | Emp -> es
  | Event ev -> es
  | Cons (es1, es2) -> 
      let normalES1 = normalES es1 pi in
      (match (normalES1, es2) with 
        (Emp, _) -> es2
      | (_, Emp) -> normalES1
      | (Bot, _) -> Bot
      | (Omega _, _ ) -> normalES1
      | (normal_es1, _) -> Cons (normal_es1, normalES es2 pi)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES es1 pi, normalES es2 pi) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (norml_es1, norml_es2) -> ESOr (norml_es1, norml_es2)
      ;)
  | Ttimes (es1, terms) -> 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ -> 
        let allPi = getAllPi pi [] in
        if existPi (Eq (terms, 0)) allPi then Emp else Ttimes (normalInside, terms))
  | Omega es1 -> 
      let normalInside = normalES es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ ->  Omega normalInside)
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

let rec normalEffect eff =
  match eff with
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
  | (Event s1, Event s2) -> s1 == s2
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

let rec reoccur piL esL piR esR delta num = 
  if num == 0 then true
  else
  match delta with 
  | [] -> false
  | (pi1, es1, pi2, es2) :: rest -> 
      if (compareEff (Effect(piL, esL)) (Effect(pi1, es1)) && compareEff (Effect(piR, esR))  (Effect(pi2, es2))) 
      then reoccur piL esL piR esR rest (num-1)

      else reoccur piL esL piR esR rest num (*REOCCUR*) 
  ;;

let rec addConstrain effect addPi =
  match effect with
    Effect (pi, eff) -> Effect (normalPure (PureAnd (pi, addPi)), eff)
  | Disj (effL1, effL2) -> Disj (addConstrain effL1 addPi, addConstrain effL2 addPi)
  ;;

let entailConstrains pi1 pi2 = askZ3 (PureAnd (pi1, pi2)) ;;

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
  | _ -> []
  ;;

let rec getAllVarFromEff es = getAllVarFromES es
(*match effect with 
    Effect (pi, es) -> getAllVarFromES es
  | Disj (eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
*)
;;

let rec getAllVarFromDelta (delta:context) acc =  
  match delta with 
    [] -> acc
  | (piL, esL, piR, esR)::rest -> 
      getAllVarFromDelta rest (append acc (append (getAllVarFromEff esL) (getAllVarFromEff esR ) ))
  ;;

(*used to generate the free veriables, for subsititution*)
let freeVar = ["t1"; "t2"; "t3"; "t4";"t5";"t6";"t7";"t8";"t9";"t10"
              ;"t11"; "t12"; "t13"; "t14";"t15";"t16";"t17";"t18";"t19";"t20"
              ;"t21"; "t22"; "t23"; "t24";"t25";"t26";"t27";"t28";"t29";"t30"];;



let rec getAfreeVar delta  =
  let bounded = getAllVarFromDelta delta [] in
  let rec findOne li = 
    match li with 
        [] -> raise ( Foo "freeVar list too small exception!")
      | x :: xs -> if (exist bounded x) == true then findOne xs else x
  in
  findOne freeVar
;;

let rec pattermMatchingTerms terms pattern termNew= 
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
  | _ -> es
  ;;

let rec substituteEff (effect:effect) (termOrigin:terms) (termNew:terms) = 
  match effect with 
    Effect (pi, es) -> Effect (pi, substituteES es termOrigin termNew) 
  | Disj (eff1, eff2) -> Disj (substituteEff eff1 termOrigin termNew , substituteEff eff2 termOrigin termNew ) 
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
  ;;


let rec quantified_in_LHS esL str = 
  match esL with
  | Ttimes (es1, term) -> quantified_by_Term term str
  | Cons (es1, es2) -> quantified_in_LHS es1 str || quantified_in_LHS es2 str
  | Omega (es1) -> quantified_in_LHS es1 str
  | ESOr (es1, es2) -> raise (Foo "quantified_in_LHS exception")
  | _ -> false
  ;;


(*-------------------------------------------------------------
--------------------Main Entrance------------------------------
---------------------------------------------------------------
This decision procedure returns a derivation tree and a boolean
value indicating the validility of the effect entailment
-------------------------------------------------------------*)

let rec containment (effL:effect) (effR:effect) (delta:context): (binary_tree * bool) = 

  let normalFormL = normalEffect effL in 
  let normalFormR = normalEffect effR in
  let showEntail  = showEntailmentEff normalFormL normalFormR in 
  let unfoldSingle ev piL esL piR esR del = 
    let derivL = derivative piL esL ev in
    let derivR = derivative piR esR ev in
    let deltaNew = append del [(piL, esL, piR, esR)] in
    let (tree, result) = containment derivL derivR deltaNew in
    (Node (showEntailmentEff (Effect(piL, esL)) (Effect(piR, esR)) ^ "   [Unfold with Fst = "^  ev ^ "]",[tree] ), result)
  in
  (*Unfold function which calls unfoldSingle*)
  let unfold del piL esL piR esR= 
    let fstL = fst piL esL in 
    let resultL = map (fun ev ->  (unfoldSingle ev piL esL piR esR del)) fstL in
    let trees = map (fun tuple -> getFst tuple ) resultL in
    let results = map (fun tuple -> getSnd tuple ) resultL in
    (*must be all the sub trees success && *)
    let result = List.fold_right (&& ) results true in  
    (Node (showEntailmentEff (Effect(piL, esL)) (Effect(piR, esR)) ,trees ), result)    
  in 
  
  match (normalFormL, normalFormR) with
    (Disj (effL1, effL2), _) -> 
    (*[LHSOR]*)
      let (tree1, re1 ) = (containment effL1 effR delta ) in
      let (tree2, re2 ) = (containment effL2 effR delta ) in
      (Node (showEntailmentEff effL effR ^ showRule LHSOR, [tree1; tree2] ), re1 && re2)
  | (_, Disj (effR1, effR2)) -> 
    (*[RHSOR]*)
      let (tree1, re1 ) = (containment effL effR1 delta ) in
      let (tree2, re2 ) = (containment effL effR2 delta ) in
      (Node (showEntailmentEff effL effR ^ showRule RHSOR, [tree1; tree2] ), re1 || re2)
  | (Effect (piL, esL), Effect (piR, esR))-> 
      
      if (nullable piL esL) == true && (nullable piR esR) == false 
      (*[DISPROVE]*)
      then (Node(showEntail ^ "   [DISPROVE]", []), false) 
      else if (isEmp normalFormR) == true  
      (*[Frame]*)
      then
        if entailConstrains piL piR 
        then (Node(showEntail^"   [Frame-Prove]" ^" with R = "^(showES esL ), []),true) 
        else (Node(showEntail ^ "   [Frame-Contra]", []),false) 
      else if (reoccur piL esL piR esR delta 2) == true  
      (*[Reoccur]*)
      then
        if entailConstrains piL (getPureFromEffect normalFormR)
        then (Node(showEntail ^ "   [Reoccur-Prove]", []), true) 
        else (Node(showEntail ^ "   [Reoccur-contra]", []), false)                      
      else 
        (match esL with
        (*LHSEX*)
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
                            let (tree1, re1 ) = (containment leftZero rightZero delta ) in
                            let (tree2, re2 ) = (containment leftNonZero rightNonZero delta ) in
                            (Node (showEntailmentEff effL effR ,[tree1; tree2] ), re1 && re2)
                | false -> (*[UNFOLD]*)unfold delta piL esL piR esR
                )
            | Plus  (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let (tree, re) = containment lhs rhs delta in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re)
            | Minus (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let (tree, re) = containment lhs rhs delta in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re)
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
                            let (tree1, re1 ) = (containment leftZero rightZero delta ) in
                            let (tree2, re2 ) =  (containment leftNonZero rightNonZero delta ) in 
                            (Node (showEntailmentEff effL effR , [tree1; tree2] ), re1 && re2)
                        | false -> (*UNFOLD*) unfold delta piL esL piR esR
                        )
              | Plus  (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in

                        containment lhs rhs delta 
              | Minus (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta 
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
                            let (tree1, re1 ) = (containment leftZero rightZero delta ) in
                            let (tree2, re2 ) = (containment leftNonZero rightNonZero delta ) in
                            (Node (showEntailmentEff effL effR ,[tree1; tree2] ), re1 || re2)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let (tree, re) = containment lhs rhs delta in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re)
                | Minus (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let (tree, re) = containment lhs rhs delta in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re)
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
                            let (tree1, re1 ) = (containment leftZero rightZero delta ) in
                            let (tree2, re2 ) =  (containment leftNonZero rightNonZero delta ) in 
                            (Node (showEntailmentEff effL effR , [tree1; tree2] ), re1 || re2)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta 
                | Minus (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta 
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
  let (tree, re) = containment  lhs rhs [] in
  let result = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  let buffur = ( "===================================="^"\n" ^(showEntailmentEff lhs rhs)^"\n[Result] " ^(if re then "Succeed" else "Fail") ^"\n\n"^ result)
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

(*let runsutes = runTestcases testcases;;

  
let deday = 
  let tick = (Event "Tick") in 
  let lightup = (Event "LightUp") in 
  let eff1 = Effect (Gt (Var "t" ,-1), Cons (Ttimes (tick, Var "t"), lightup)) in 

  let eff1_1 = Cons (Ttimes (tick, (Minus(Var "t",1))), lightup) in 
  let effIF = Effect (Eq (Var "t" ,0), lightup) in
  let elseF1 = Effect (PureOr(Gt (Var "t" ,0),Lt (Var "t" ,0)), Cons(tick, eff1_1)) in
  let elseF2 = Effect(PureOr(Gt (Var "t" ,0),Lt (Var "t" ,0)) , Cons (tick,Omega (tick))) in 
  let effELSE = Disj (elseF1 , elseF2) in
  let eff0 = Disj(effIF, effELSE) in
  let effect_delay = Disj (eff1, elseF2) in
  
  let lhs = eff0 in 
  let rhs = effect_delay in
  
  printReport lhs rhs ;;
*)

  (*
  1, parser
  2, website
  2, comparasion
  *)



(*

ocamllex lexer.mll
menhir parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c Askz3.ml
ocamlc -c Rewriting.ml
ocamlc -o trs parser.cmo lexer.cmo Askz3.cmo Rewriting.cmo

sudo ocamllex lexer.mll
sudo menhir parser.mly
sudo ocamlc -c ast.mli
sudo ocamlc -c parser.mli
sudo ocamlc -c parser.ml
sudo ocamlc -c lexer.ml
sudo ocamlc -c Askz3.ml
sudo ocamlc -c Rewriting.ml
sudo ocamlc -o trs parser.cmo lexer.cmo Askz3.cmo Rewriting.cmo

ocamlfind ocamlopt -o trs -cclib -lstdc++ -thread -package z3 -linkpkg Tree.ml Askz3.ml Rewriting.ml parser.ml lexer.ml EffectParse.ml

1. containment, entilment, term rewriting system.
2. not complete.



*)
