open String
open List
open Ast
open Printf
open Parser
open Lexer
open Askz3
open Pretty
open Int32



(*
ocamlc -o trs  Tree.ml  Rewriting.ml
*)

(*----------------------------------------------------
------------------Utility Functions------------------
----------------------------------------------------*)
exception Foo of string

module CS = Set.Make(Int32) (*context set*)

type ctxSet = (CS.t * CS.t) list


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

let rec nullable (pi :pure) (es:es) : bool=
  match es with
    Bot -> false 
  | Emp -> true
  | Event _ -> false 
  | Cons (es1 , es2) -> (nullable pi es1) && (nullable pi es2)
  | ESOr (es1 , es2) -> (nullable pi es1) || (nullable pi es2)
  | ESAnd (es1 , es2) -> (nullable pi es1) && (nullable pi es2)
  | Ttimes (es1, t) -> askZ3 (PureAnd (pi, Eq (t, Number 0))) 
  | Omega es1 -> false
  | Underline -> false
  | Kleene es1 -> true
  | Not es1 -> 
   
    (match es1 with 
      Emp -> false 
    | _ -> nullable pi es1
    )

        (* 
    not (nullable pi es1)
        *)
  | Range (esList) -> 
    (let range = List.fold_left (fun acc a -> acc || (nullable pi a)) false esList in 
    range 
    )
;;

let rec getSize (es:es) : int=
  match es with
    Bot -> 0 
  | Emp -> 1
  | Event _ -> 1 
  | Cons (es1 , es2) ->  (getSize es1) + (getSize es2)
  | ESOr (es1 , es2) ->  max (getSize es1)  (getSize es2)
  | ESAnd (es1 , es2) -> max (getSize es1) (getSize es2)
  | Ttimes (es1, t) ->  (getSize es1)
  | Omega es1 ->  (getSize es1)
  | Underline -> 1
  | Kleene es1 -> (getSize es1)
  | Not es1 -> (getSize es1)
  | Range (esList) -> 
    raise (Foo "getSize range")
;;

let rec simpleFst (es:es): event = 
  match es with 
  | Event (str, p) ->  str
  | Cons (es1 , es2) ->  simpleFst es1
  | Underline -> "_"
  | _ -> raise (Foo (showES es^"simpleFst exception"))
  
  ;;

let rec isBotES (es:es) :bool = 
  match es with 
    Bot -> true 
  | _ -> false 
;;

let rec isEmpES (es:es) :bool = 
  match es with 
    Emp -> true 
  | _ -> false 
;;



let rec fst (pi :pure) (es:es): (event* int option) list = 
  let rec common (left:(string* int option) list) (right:(string*int option) list) (acc:(string*int option) list): (string*int option) list =
    match left with 
      [] -> acc 
    | x :: xs -> 
      let rec oneOF (ele:(string*int option )) (li:(string*int option) list):bool =
        (match li with 
          [] -> false 
        | y::ys -> if compareEvent ele y then true else oneOF ele ys
        )
      in 
      if oneOF x right then common xs right (append acc [x]) else common xs right acc
  in 
  match es with
    Bot -> []
  | Emp -> []
  | Event (str, p) ->  [(str, p)]
  | Omega es1 -> fst pi es1
  | Ttimes (es1, t) -> fst pi es1
  | Cons (es1 , es2) ->  if nullable pi es1 then append (fst pi es1) (fst pi es2) else fst pi es1
  | ESOr (es1, es2) -> append (fst pi es1) (fst pi es2)
  | ESAnd (es1, es2) -> common (fst pi es1) (fst pi es2) []
  | Underline -> [("_",None)]
  | Kleene es1 -> fst pi es1
  | Not es1 -> fst pi es1
  | Range (esList) -> 
  (let range = List.fold_left (fun acc a -> append acc (fst pi a)) [] esList in 
  range 
  )
;;

let rec appendEff_ES eff es = 
  match eff with 
    Effect (p , es_eff) ->  Effect(p, Cons (es_eff, es))
  | Disj (eff1 , eff2)  ->  Disj (appendEff_ES eff1 es, appendEff_ES eff2 es)
  
  (*raise ( Foo "appendEff_ES exception!")*)
  ;;

let ifShouldDisj (temp1:effect) (temp2:effect) : effect = 
  match (temp1, temp2) with
      (Effect(pure1, evs1), Effect(pure2, evs2)) -> 
        if comparePure pure1 pure2 then  Effect (pure1, ESOr (evs1, evs2))
        else Disj (temp1, temp2 )
      | _ -> 
      Disj (temp1, temp2 )
  ;;
let rec ifShouldConj (temp1:effect) (temp2:effect) : effect = 
  match (temp1, temp2) with
      (Effect(pure1, evs1), Effect(pure2, evs2)) -> 
        Effect (PureAnd (pure1, pure2), ESAnd (evs1, evs2))
    | (Effect(pure1, evs1), Disj (eff1, eff2)) -> 
      Disj (ifShouldConj temp1 eff1, ifShouldConj temp1 eff2)
    | (Disj (eff1, eff2), _) ->
      Disj (ifShouldConj eff1 temp2, ifShouldConj eff2 temp2)
      (*Disj (temp1, temp2 )*)
  ;;

let rec compareES es1 es2 = 
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
  | (Event (s1,p1), Event (s2,p2)) -> 
    compareEvent (s1,p1) (s2,p2)
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> (compareES es1L es2L) && (compareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      let one = ((compareES es1L es2L) && (compareES es1R es2R)) in
      let two =  ((compareES es1L es2R) && (compareES es1R es2L)) in 
      one || two
  | (ESAnd (es1L, es1R), ESAnd (es2L, es2R)) -> 
      let one = ((compareES es1L es2L) && (compareES es1R es2R)) in
      let two =  ((compareES es1L es2R) && (compareES es1R es2L)) in 
      one || two
  | (Omega esL, Omega esR) ->compareES esL esR
  | (Ttimes (esL, termL), Ttimes (esR, termR)) -> 
      let insideEq = (compareES esL esR) in
      let termEq = compareTerm termL termR in
      insideEq && termEq
  | (Kleene esL, Kleene esR) -> compareES esL esR
  | (Not esL, Not esR) -> compareES esL esR
  | (Underline, Underline ) -> true
  | (Range (esList1), Range (esList2)) ->  subESsetOf esList1 esList2 && subESsetOf esList2 esList1
  | _ -> false
;;

let rec compareEff eff1 eff2 =
  match (eff1, eff2) with
  | (Effect(FALSE, _ ), Effect(FALSE, _)) -> true 
  | (Effect(FALSE, _ ), Effect(_, Bot )) -> true 
  | (Effect(_, Bot), Effect(FALSE, _ )) -> true 
  | (Effect(_, Bot ), Effect(_, Bot)) -> true 

  | (Effect (pi1, es1), Effect (pi2, es2 )) -> compareES es1 es2
  | (Disj (eff11, eff12), Disj (eff21, eff22)) -> 
      let one =  (compareEff eff11  eff21) && (compareEff eff12  eff22) in
      let two =  (compareEff eff11  eff22) && (compareEff eff12  eff21 ) in
      one || two
  | _ -> false
  ;;

let rec splitEffects eff : (pure * es) list = 
  match eff with 
    Effect (p1, es1) -> [(p1, es1)]
  | Disj (eff1, eff2) -> append (splitEffects eff1) (splitEffects eff2)
  ;;

(*this is use to keep the bot in head for negation usage *)
let rec normalES_Bot es pi = 
  match es with
    Bot -> es
  | Emp -> es
  | Event _ -> es
  | Underline -> Underline
  | Cons (Cons (esIn1, esIn2), es2)-> normalES_Bot (Cons (esIn1, Cons (esIn2, es2))) pi
  | Cons (es1, es2) -> 
      let normalES1 = normalES_Bot es1 pi in
      let normalES2 = normalES_Bot es2 pi in
      (match (normalES1, normalES2) with 
        (Emp, _) -> normalES2
      | (_, Emp) -> normalES1
      | (Omega _, _ ) -> normalES1

      | (Kleene (esIn1), Kleene (esIn2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2)
      | (Kleene (esIn1), Cons(Kleene (esIn2), es2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2) 

      | (normal_es1, normal_es2) -> 
        match (normal_es1, normal_es2) with 
        |  (Cons (esIn1, esIn2), es2)-> normalES_Bot (Cons (esIn1, Cons (esIn2, es2))) pi 
        |  (ESOr (or1, or2), es2) -> normalES_Bot (ESOr ( (Cons (or1, es2)),  (Cons (or2, es2)))) pi
        |  (es1, ESOr (or1, or2)) -> normalES_Bot (ESOr ( (Cons (es1, or1)),  (Cons (es1, or2)))) pi
        | _-> Cons (normal_es1, normal_es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES_Bot es1 pi, normalES_Bot es2 pi) with 
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

  | ESAnd (es1, es2) -> 
      (match (normalES_Bot es1 pi, normalES_Bot es2 pi) with 

      | (Bot, norml_es2) -> Bot
      | (norml_es1, Bot) -> Bot
      | (Event (s1, p1), Event (s2, p2)) -> if compareEvent (s1, p1) (s2, p2) then Event (s1, p1) else Bot

      | (Emp, norml_es2) -> if nullable pi norml_es2 then Emp else Bot 
      | (norml_es1, Emp) -> if nullable pi norml_es1 then Emp else Bot 

      | (norml_es1, norml_es2) -> 
        if aCompareES  norml_es1 norml_es2 == true then norml_es1
        else ESAnd (norml_es1, norml_es2)

      )



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
      let normalInside = normalES_Bot es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ -> 
        let allPi = getAllPi pi [] in 
        if (existPi (Eq (terms, Number 0)) allPi) || (compareTerm t (Number 0 )) then Emp else Ttimes (normalInside, t))
        (*else if (existPi (Eq (terms, n)) allPi)) then Emp else Ttimes (normalInside, t))*)
  | Omega es1 -> 
      let normalInside = normalES_Bot es1 pi in 
      (match normalInside with
        Emp -> Emp
      | _ ->  Omega normalInside)
  | Kleene es1 -> 
      let normalInside = normalES_Bot es1 pi in 
      (match normalInside with
        Emp -> Emp
      | Kleene esIn1 ->  Kleene (normalES_Bot esIn1 pi)
      | ESOr(Emp, aa) -> Kleene aa
      | _ ->  Kleene normalInside)

  | Range (esList) -> 
      (let range = List.map (fun a -> normalES_Bot a pi) esList in 
       List.fold_left (fun acc a -> ESOr (acc, a)) Bot range 
      )

  | Not esIn -> 
      match esIn with 
        ESOr (esIn1, esIn2) -> ESAnd (Not esIn1, Not esIn2)
      | ESAnd (esIn1, esIn2) -> ESOr (Not esIn1, Not esIn2)
      | Not esIn1 -> esIn1
      | Bot -> Underline
      | Emp -> Bot
      | _ -> Not esIn (*raise (Foo "I have not thought through! Not in normal")*)
  ;;

let rec normalES es pi = 
  match es with
    Bot -> es
  | Emp -> es
  | Event _ -> es
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

  | ESAnd (es1, es2) -> 
      (match (normalES es1 pi, normalES es2 pi) with 

      | (Bot, norml_es2) -> Bot
      | (norml_es1, Bot) -> Bot
      | (Event (s1, p1), Event (s2, p2)) -> if compareEvent (s1, p1) (s2, p2) then Event (s1, p1) else Bot

      | (Emp, norml_es2) -> if nullable pi norml_es2 then Emp else Bot 
      | (norml_es1, Emp) -> if nullable pi norml_es1 then Emp else Bot 

      | (norml_es1, norml_es2) -> 
        if aCompareES  norml_es1 norml_es2 == true then norml_es1
        else ESAnd (norml_es1, norml_es2)

      )



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
       List.fold_left (fun acc a -> ESOr (acc, a)) Bot range 
      )

  | Not esARG -> 
      let esIn = normalES  esARG pi in 
      match esIn with 
        ESOr (esIn1, esIn2) -> ESAnd (Not esIn1, Not esIn2)
      | ESAnd (esIn1, esIn2) -> ESOr (Not esIn1, Not esIn2)
      | Not esIn1 -> esIn1
      (*| Bot -> Underline
      | Emp -> Bot*)
      | _ -> Not esIn (*raise (Foo "I have not thought through! Not in normal")*)
  ;;

let rec normalEffect eff =
  let noPureOr  = deletePureOrInEff eff in 
  match noPureOr with
    Effect (p, es) -> 
      if (askZ3 p) == false then 
        ( 
          (*print_string (showPure p^"   "^ showES es^ "\n 11********\n");*)
          Effect (FALSE, normalES es p)
        )
      else 
        let p_normal = normalPure p in 
        let es_normal  = normalES es p in
        (match es_normal with 
          ESOr (es_nor1, es_nor2) -> Disj (Effect (p_normal, es_nor1), Effect (p_normal, es_nor2))
        | _ -> Effect ( p_normal, es_normal)
        )
  | Disj (eff1, eff2) -> 
      match (normalEffect eff1, normalEffect eff2) with
        (Effect (_,  Bot  ), _) -> normalEffect eff2
      | (_, Effect (_,  Bot)) -> normalEffect eff1
      | (Effect (FALSE,  _), _) -> normalEffect eff2
      | (_, Effect (FALSE,  _)) -> normalEffect eff1

      | (Disj(eff1In, eff2In), norml_eff2 ) ->
        if compareEff norml_eff2 eff1In || compareEff norml_eff2 eff2In then Disj(eff1In, eff2In)
        else Disj (Disj(eff1In, eff2In), norml_eff2 )
      | (norml_eff2, Disj(eff1In, eff2In) ) ->
        if compareEff norml_eff2 eff1In || compareEff norml_eff2 eff2In then Disj(eff1In, eff2In)
        else Disj (norml_eff2, Disj(eff1In, eff2In))

      | _ -> Disj (normalEffect eff1, normalEffect eff2)
  ;;

let trunItIntoWideCard (pi:pure) (esIn: es) : es = 
  let rec helper (esI: es) : es = 
    match esI with 
      Event (ev1, p1) -> Underline 
    | Underline -> Underline
    | Ttimes (es1, t) -> Ttimes (helper es1, t)
    | Omega es1 -> Omega (helper es1)
    | Kleene es1 -> Kleene (helper es1)
    | Cons (es1, es2) -> Cons (helper es1, helper es2)
    | Range _ -> raise (Foo "trunItIntoWideCard Range")
    | Bot -> raise (Foo "trunItIntoWideCard Bot")
    | ESAnd (es1 , es2) -> raise (Foo "trunItIntoWideCard ESAnd")
    | ESOr (es1 , es2) -> raise (Foo "trunItIntoWideCard ESOr")
    | Emp -> Bot
    | Not _ -> raise (Foo "trunItIntoWideCard Not")
  in 
  match normalES_Bot esIn pi with
    Cons (Bot, essss) ->   helper essss
  | Bot -> Emp
  | _ -> 
  (*print_string (showES esIn);*)
  raise (Foo "trunItIntoWideCard")
;;
    
let rec simpleDerivative (ev:event) (es:es) :es = 
  match es with
    Emp -> Bot
  | Event (ev1, p1) -> 
      if String.compare ev "_" ==0 then  Emp
      else if String.compare ev ev1 ==0 then Emp else Bot
  | Cons (es1 , es2) ->  Cons (simpleDerivative ev es1 , es2)
  | Underline -> Emp
  | _ -> raise (Foo (showES es^"simpleDerivative exception"))

  ;;



let rec derivative (p :pure) (es:es) (varL: var list) (ev:(string*int option)): (effect) =
  match es with
    Bot -> Effect (p,  Bot)
  | Emp -> Effect (p,  Bot)
  | Underline -> Effect (p, Emp)
  | Event (ev1, p1) -> 
      if compareEvent ev ("_", None) then  Effect (p, Emp)
      else if compareEvent (ev1, p1) ev then Effect (p, Emp) else Effect (p, Bot)
  | Omega es1 -> appendEff_ES (derivative p es1 varL ev) es
  | ESOr (es1 , es2) -> 
    let temp1 = normalEffect (derivative p es1 varL ev) in
    let temp2 = normalEffect (derivative p es2 varL ev) in 
    ifShouldDisj temp1 temp2
  | ESAnd (es1 , es2) -> 
    let temp1 = normalEffect (derivative p es1 varL ev) in
    let temp2 = normalEffect (derivative p es2 varL ev) in 
    ifShouldConj temp1 temp2
  | Ttimes (es1, t) -> 
      let pi = PureAnd (Gt (t, Number 0), p) in
      let efF = derivative pi es1 varL ev in 
      let esT_minus1 = Ttimes (es1,  Minus (t, Number 1)) in
      appendEff_ES efF esT_minus1
  | Cons (es1 , es2) -> 
      if nullable p es1 
      then let efF = derivative p es1 varL ev in 
          let effL = normalEffect (appendEff_ES efF es2) in 
          let effR = normalEffect (derivative p es2 varL ev) in 
          ifShouldDisj effL effR
      else let efF = derivative p es1 varL ev in 
          appendEff_ES efF es2    
          
  | Kleene es1 -> appendEff_ES  (derivative p es1 varL ev) es
  | Not es1 -> 

    (let tryder = normalEffect (derivative p es1 varL ev) in 

    match  tryder with
    
      Effect (ppp,Bot) -> Effect (ppp, Kleene(Underline))(*Effect (ppp,Emp)*)
    | Effect (ppp,Emp) -> Effect (ppp,Bot)
    (*
        let newVar = getAfreeVar varL in
        Effect (PureAnd (ppp, Gt (Var newVar, Number 0)), ESOr (Ttimes (Underline, Var newVar), Omega (Underline) ))
        *)
    | _ -> 
      (let rec helper (noteffect:effect) : effect = 
        match noteffect with 
          Effect (pi, esnot) ->  Effect (pi, Not esnot)
        | Disj (eff11, eff22) -> Disj (helper eff11, helper eff22)
      in 
      helper tryder)
    )
    
  | Range (esList) -> 
      (let range = List.map (fun a -> derivative p a varL ev) esList in 
      let final = List.fold_left (fun acc a -> Disj (acc, a)) (Effect(FALSE, Bot)) range in 
      final 
  )

;;


(*----------------------------------------------------
----------------------CONTAINMENT--------------------
----------------------------------------------------*)



let rec reoccurCtxSet (esL:CS.t) (esR:CS.t) (ctx:ctxSet) = 
  match ctx with 
  | [] -> false 
  | (es1, es2) :: rest -> 

    if (CS.subset esL es1 && CS.subset es2 esR ) then 
    (
      (*print_string ("\n=======\n");
      CS.iter (fun a -> print_string (to_string a ^"  ")) es1;
      print_string ("\n");
      CS.iter (fun a -> print_string (to_string a ^"  ")) es2;
      print_string ("\n-------\n");
      CS.iter (fun a -> print_string (to_string a ^"  ")) esL;
      print_string ("\n");
      CS.iter (fun a -> print_string (to_string a ^"  ")) esR;
      print_string ("\n");
      *)
      true
    )
    else reoccurCtxSet esL esR rest (*REOCCUR*) 
  ;;

let rec splitCons (es:es) : es list = 

  match es with 
    ESOr (es1, es2) -> append (splitCons es1) (splitCons es2)
  | _ -> [es]

  ;;

let fromEsToSet (es:es): CS.t = 

  let listL = List.map (fun a -> 
    let temp = regToInt a in 
    (
    (*print_string (showES a);
    print_string (to_string temp ^"\n");*)
    temp
    )
    ) (splitCons es) in 
  List.fold_left (fun acc a -> CS.union acc (CS.singleton a)) CS.empty listL
  ;;

let fromListToSet (esL:es list) :CS.t = 
  let listL = List.map (fun a -> regToInt a) ( esL) in 
  List.fold_left (fun acc a -> CS.union acc (CS.singleton a)) CS.empty listL
  ;;

let rec remove_dup lst= 
  match lst with
      | [] -> []
      | h::t -> h::(remove_dup (List.filter (fun x -> x<>h) t))
      ;;

let rec connectDisj (esL:es list) :es = 
  match esL with 
    [] -> Bot
  | [x] -> x
  | x::xs -> ESOr (x, connectDisj xs )
  ;;

let rec reoccurHelp piL esL piR esR (del:context) = 
  match del with 
  | [] -> false 
  | (pi1, es1, pi2, es2) :: rest -> 
    if (compareEff (Effect(piL, esL)) (Effect(pi1, es1)) && compareEff (Effect(piR, esR))  (Effect(pi2, es2))) 
    then true

    else reoccurHelp piL esL piR esR rest (*REOCCUR*) 
  ;;


  

let rec reoccur piL esL piR esR (delta:context list) = 
  match delta with 
  | [] -> false
  | [x] -> false
  | del:: rest -> 
      if (reoccurHelp piL esL piR esR del) == true then true 
      else reoccur piL esL piR esR rest (*REOCCUR*) 
  ;;

let rec dropLastDelta (delta:context list):context list =
  match  delta with 
  | [] -> [] 
  | [x] -> []
  | x :: xs -> x :: (dropLastDelta xs )
  ;;

  (*
type hypoGraph = HypoNode of pure * es 
               | HypoPar of pure * es * hypoGraph list

  

let rec constructHypoGraph (delta:context) (li: hypoGraph list): hypoGraph list  =
  let addHypoInGraphList (li': hypoGraph list) (p1, es1, p2, es2) : hypoGraph list  =
    match li' with 
    | [] -> [HypoPar (p1, es1, [HypoNode (p2, es2)])]
    | x::xs -> addHypoInGraph (p1, es1, p2, es2)
  in 
  match delta with 
  | [] -> li 
  | x :: xs -> 
    let newLi = addHypoInGraphList li x in 
    constructHypoGraph xs newLi
  ;;
*)
let rec transitivityHelper piL esL piR esR (del:context) :bool = 
  let rec helper (del':context ) piL' esL' = 
    match del' with 
      [] -> None 
    | (pi1, es1, pi2, es2) :: rest -> 
      if (compareEff (Effect(piL', esL')) (Effect(pi1, es1))) then Some (pi2, es2)
      else helper rest piL' esL'
  in
  match helper del piL esL with 
    None -> false 
  | Some (pi2, es2) -> 
      (match reoccurHelp pi2 es2 piR esR del with
        true ->  true 
      | false -> transitivityHelper pi2 es2 piR esR del
      )

let rec transitivity piL esL piR esR (del:context list) :bool = 
  let realHypo = flatten (dropLastDelta del) in  
  (*let graph = constructHypoGraph realHypo [] in 
  false*)
  transitivityHelper piL esL piR esR realHypo
  ;;
  





let rec getPureFromEffect effect = 
  match effect with
    Effect (pi, _) -> pi
  | Disj (eff1, eff2) -> PureOr ((getPureFromEffect eff1), (getPureFromEffect eff2))
  ;;

let rec getAllVarFromTerm term = 
  match term with 
    Var str -> [str]
  | Number _ -> []
  | Plus (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
  | Minus (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
  ;;

let rec getAllVarFromPi (pi:pure): string list  = 
    match pi with 
      TRUE -> []
    | FALSE -> []
    | Gt (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
    | Lt (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
    | GtEq (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
    | LtEq (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
    | Eq (t1, t2) -> append (getAllVarFromTerm t1) (getAllVarFromTerm t2)
    | PureOr (p1, p2) -> append (getAllVarFromPi p1) (getAllVarFromPi p2)
    | PureAnd (p1, p2) -> append (getAllVarFromPi p1) (getAllVarFromPi p2)
    | Neg p1 -> getAllVarFromPi p1
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
    Effect (pi, es) -> append (getAllVarFromES es)  (getAllVarFromPi pi)
  | Disj(eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
(*match effect with 
    Effect (pi, es) -> getAllVarFromES es
  | Disj (eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
*)
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

(*
let rec enForcePure eff1 eff2 = 
  match eff1 with 
    Effect (pi1, es1) ->
      (match eff2 with 
        Effect (pi2, es2) -> Effect(PureAnd (pi1, pi2), es2)
      | Disj (eff_1, eff_2) -> Disj (enForcePure eff1 eff_1, enForcePure eff1 eff_2)
      ) 
  | Disj (_,_) -> raise (Foo "enForcePure exception")
  ;;
*)
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
    | Not esIn -> getFirstVar esIn
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

let rec existialRHSEff piL esL effR varList :bool = 
  match effR with 
    Effect (piR, esR) -> existialRHS piL esL esR varList
  | Disj (eff1, eff2) -> 
      if existialRHSEff piL esL eff1 varList then true else  existialRHSEff piL esL eff2 varList

;;

let rec remove_dups lst= 
  match lst with
      | [] -> []
      | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
      ;;





(*
let getInstansVal piL esL: int list = 
  let rec getValFromTerm term = 
    match term with 
      Var str -> []
    | Number n -> [n] 
    | Plus (tt, n) -> getValFromTerm tt 
    | Minus (tt, n) -> getValFromTerm tt 
  in 
  let rec getAllValFromES (es :es): int list = 
    match es with 
      Cons (es1, es2) -> append (getAllValFromES es1) (getAllValFromES es2)
    | ESOr (es1, es2) -> append (getAllValFromES es1) (getAllValFromES es2)
    | Ttimes (esIn, t) -> getValFromTerm t 
    | Kleene esIn -> getAllValFromES esIn
    | Omega esIn -> getAllValFromES esIn
    | _ -> []
  in 
  let rec getAllValFromPure (pi :pure): int list = 
    match pi with 
      Gt (tt, n) -> getValFromTerm tt 
    | Lt (tt, n) -> getValFromTerm tt 
    | Eq (tt, n) -> getValFromTerm tt 
    | PureOr (p1, p2) -> append (getAllValFromPure p1) (getAllValFromPure p2) 
    | PureAnd (p1, p2) -> append (getAllValFromPure p1) (getAllValFromPure p2) 
    | Neg p1 -> (getAllValFromPure p1) 
    | _ -> []
  in 
  remove_dups (append (getAllValFromPure piL) (getAllValFromES esL))
  ;;
*)

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
  | Event _  -> es
  | Cons (es1, es2) ->  Cons (substituteESWithVal es1 var1 val1, substituteESWithVal es2 var1 val1)
  | ESOr (es1, es2) ->  ESOr (substituteESWithVal es1 var1 val1, substituteESWithVal es2 var1 val1)
  | ESAnd (es1, es2) ->  ESAnd (substituteESWithVal es1 var1 val1, substituteESWithVal es2 var1 val1)

  | Ttimes (esIn, t) -> Ttimes (substituteESWithVal esIn var1 val1, substituteTermWithVal t var1 val1)
  | Kleene esIn -> Kleene (substituteESWithVal esIn var1 val1)
  | Omega esIn -> Omega (substituteESWithVal esIn var1 val1)
  | Not esIn -> Not (substituteESWithVal esIn var1 val1)
  | Underline -> es
  | Range (esList) -> 
      (let range = List.map (fun a -> substituteESWithVal a var1 val1) esList in 
      let final = List.fold_left (fun acc a -> ESOr (acc, a)) Bot range in 
      final 
  )
  ;;

let instantiateEff (pi:pure) (es:es) (instances: int list): (effect*int) list = 
  match getFirstVar es with 
    None -> []
  | Some (str) ->  
    map (fun n -> 
     (Effect (pi, substituteESWithVal es str n), n)) instances 
  ;;

let get_0 (a, b) = a;;

let rec instantiateEffR  (effR:effect) (instances: int list): (effect*int) list = 
  match effR with 
    Effect (piR, esR) ->  instantiateEff piR esR instances
  | Disj (eff1, eff2) -> 
    List.fold_right (fun instance acc -> 
      let temp1 = instantiateEffR eff1 [instance] in 
      let temp2 = instantiateEffR eff2 [instance] in 
      match (List.length temp1, List.length temp2) with 
        | (0, 0) ->acc
        | (0, _) -> append acc [(get_0 (hd temp2), instance)]
        | (_, 0) -> append acc [(get_0 (hd temp1), instance)]
        | _ -> append acc [(Disj(get_0 (hd temp1), get_0 (hd temp2)), instance)]
       ) (instances) [] 

  ;;

  (*
let rec getProductHypo (eff1:effect) (eff2:effect) : context = 
  let eff1n = normalEffect eff1 in 
  let eff2n = normalEffect eff2 in 
  match eff1n with 
    Effect (pi1, es1) -> 
        (match eff2n with 
          Effect (pi2, es2) -> [(pi1, es1, pi2, es2)] 
          | Disj (eff2InL, eff2InR) -> append (getProductHypo eff1 eff2InL) (getProductHypo eff1 eff2InR) 
        )
  | Disj (eff1InL, eff1InR) -> append (getProductHypo eff1InL eff2) (getProductHypo eff1InR eff2) 
;;
*)
(*
let getNewHypos (fstL:string list) (piL:pure) (esL:es) (piR:pure) (esR:es) : context = 
  let effPair  = map (fun ev -> ( (derivative piL esL ev), (derivative piR esR ev))) fstL in 
  let pi_EsPair = flatten (map (fun (eff1, eff2) -> getProductHypo eff1 eff2 ) effPair) in 
  pi_EsPair
  ;;

*)
(*-------------------------------------------------------------
--------------------Main Entrance------------------------------
---------------------------------------------------------------
This decision procedure returns a derivation tree and a boolean
value indicating the validility of the effect entailment
-------------------------------------------------------------*)



let effectEntailSyntatically eff1 eff2 :bool =
  let effsL = splitEffects eff1 in 
  let effsR = splitEffects eff2 in
  let rec checkSingle piL esL liR:bool = 
    match liR with
      [] -> false  
    | (piR, esR)::xs -> if compareES esL esR then true else checkSingle piL esL xs
  in 
  List.fold_right (fun (piL, esL) acc -> acc && checkSingle piL esL effsR) (effsL) true 
  ;;

let rec checkReoccur (effL:effect) (effR:effect) (delta:hypotheses) :bool =
  let checkSingle (hypoL:effect) (hypoR:effect) = 
    effectEntailSyntatically effL hypoL && effectEntailSyntatically hypoR effR 
  in 
  match delta with
    [] -> false 
  | (hyL, hyR)::xs -> 
    if checkSingle hyL hyR then true else checkReoccur effL effR xs
  ;;

let rec checkNullable (eff:effect) :bool = 
  match eff with
    Effect (pi, es) -> nullable pi es
  | Disj (eff1, eff2) -> checkNullable eff1 || checkNullable eff2 
 ;;


let rec checkFst (eff:effect) : (event*int option) list = 
  match eff with
    Effect (pi, es) -> fst pi es
  | Disj (eff1, eff2) -> append (checkFst eff1) (checkFst eff2) 
 ;;

let rec checkDerivative  (eff:effect) (ev:(event*int option)) (varL :var list): effect = 
  match eff with 
    Effect (pi, es) -> derivative pi es varL ev
  | Disj (eff1, eff2) -> Disj (checkDerivative eff1 ev varL, checkDerivative eff2 ev varL)
  ;;


let rec pureUnion (eff :effect ):pure = 
  let effs = splitEffects eff in
  List.fold_right (fun (piL, esL) acc -> PureOr(acc, piL)) (effs) FALSE 
;;

let rec headEs (es:es) : es list =
  match es with
    Cons (es1 , es2) -> headEs es1
  | Kleene es1 -> headEs es1
  | Omega es1 -> headEs es1
  | Not es1 -> headEs es1
  | ESOr (es1, es2 ) -> append (headEs es1) (headEs es2)
  | _ -> [es]
  ;;

let rec subsetOf (small : string list) (big : string list) :bool = 
  let rec oneOf a set :bool = 
    match set with 
      [] -> false 
    | y:: ys -> if String.compare a y == 0 then true else oneOf a ys
  in 
  match small with 
    [] -> true 
  | x :: xs -> if oneOf x big == false then false else subsetOf xs big
;;


let rec makeList min max acc: int list = 
  if min  < max then makeList (min +1) max (append acc [min])
  else append acc [max]
;;


let rec headEff (eff:effect) : es list = 
  match eff with 
    Effect (pi, es) -> headEs es
  | Disj (eff1, eff2) -> append (headEff eff1) (headEff eff2)
  ;; 

let needToBeInstantiated eff varLi :bool = 
  let headsofRHS = headEff eff in 
  List.exists (fun a -> not (subsetOf (getAllVarFromES a) varLi)) headsofRHS 
;;

let rec getTheheadneedToBeInstantiated headsofRHS varList:es = 
  match headsofRHS with 
    [] -> raise (Foo "getTheheadneedToBeInstantiated")
  | x :: xs -> if subsetOf (getAllVarFromES x) varList == false then x else getTheheadneedToBeInstantiated xs varList
  ;;

let rec addEntailConstrain (eff:effect) (pi:pure) :effect = 
  match eff with 
    Effect (pi1, es1)  -> 
      (match entailConstrains pi pi1 with 
        true -> eff
      | false -> Effect (FALSE, es1)
      )
  | Disj (eff1, eff2) -> Disj(addEntailConstrain eff1 pi, addEntailConstrain eff2 pi)
  ;;


let rec isInfinite (es:es) :bool = 
  match es with 
  Omega es1 -> true
| Cons (es1 , es2) -> (isInfinite es2)
| ESOr (es1 , es2) -> (isInfinite es1) && (isInfinite es2)
| ESAnd (es1 , es2) -> (isInfinite es1) && (isInfinite es2)
| _ ->  false 

  ;;


let rec keepIfinite (eff:effect):effect =
  match eff with 
    Effect (pi, es) -> if isInfinite es then eff else Effect (FALSE, Bot)
  | Disj (eff1, eff2) -> Disj (keepIfinite eff1, keepIfinite eff2)
;;

let rec notSureInfinite (eff:effect):bool =
  match eff with 
    Effect (pi, es) -> not (isInfinite es) 
  | Disj (eff1, eff2) -> notSureInfinite eff1 || notSureInfinite eff2
;;

let rec itStartsFromANegationES (es:es) :bool = 
    match es with
    Cons (es1 , es2) -> itStartsFromANegationES es1
  | Kleene es1 -> itStartsFromANegationES es1
  | Omega es1 -> itStartsFromANegationES es1
  | Not es1 -> true
  | ESOr (es1, es2 ) -> (itStartsFromANegationES es1)|| (itStartsFromANegationES es2)
  | _ -> false
  ;;

let rec itStartsFromANegation (eff:effect):bool = 
  match eff with 
    Effect (pi, es) -> itStartsFromANegationES es 
  | Disj (eff1, eff2) -> itStartsFromANegation eff1 || itStartsFromANegation eff2
  ;;

(*mode = 1  means ential without resedue*)
let rec containment1 (effL:effect) (effR:effect) (delta:hypotheses) (mode:bool): (binary_tree * bool * int) = 
  (*
  let startTimeStamp = Sys.time() in
  *)
  
  let normalFormL = normalEffect effL in 
  let normalFormR = normalEffect effR in
  (*
  let verification_time = "[normalEffect Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in

  print_string (verification_time);
  *)
  let varList = getAllVarFromEff normalFormL in 
  let showEntail  = (*showEntailmentEff effL effR ^ " ->>>> " ^*)showEntailmentEff normalFormL normalFormR in 
  (*
  print_string("\n================\n"^showEntail ^"\n");
  print_string (List.fold_left (fun acc a  -> acc ^ " " ^ a ^ "\n") ""  varList);
  *)
  let unfold eff1 eff2 del = 
    let fstL = checkFst eff1 in 
    let deltaNew = append del [(eff1, eff2)] in

    let rec chceckResultAND li acc staacc:(bool *binary_tree list* int )=
      (match li with 
        [] -> (true, acc, staacc) 
      | ev::fs -> 
          let deriL = checkDerivative eff1 ev varList in
          let deriR = checkDerivative eff2 ev varList in
          let (tree, re, states) =  containment1 deriL deriR deltaNew mode in 
          if re == false then (false , tree::acc, staacc+states)
          else chceckResultAND fs (tree::acc) (staacc+states)
      )
    in 
    let (resultFinal, trees, states) = chceckResultAND fstL [] 0 in 
    (Node (showEntail ^ "   [UNFOLD]",trees ), resultFinal, states+1)    
  in 
  match (normalFormL, normalFormR) with 
      (*this means the assertion or precondition is already fail*)
    (Effect(FALSE, _), _) -> (Node(showEntail ^ "   [Bot-LHS]", []), true, 0)  
  | (Effect(_, Bot), _) -> (Node(showEntail ^ "   [Bot-LHS]", []), true, 0)  
  | (_, Effect(FALSE, _)) -> (Node(showEntail ^ "   [DISPROVE]", []), false, 0)  
  | (_, Effect(_, Bot)) -> (Node(showEntail ^ "   [DISPROVE]", []), false, 0)  
  
  | (Disj (effL1, effL2), _) -> 
    (*[LHSOR]*)
      let (tree1, re1, states1 ) = (containment1 effL1 effR delta mode) in
      if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree1] ),  false, states1)
      else 
        let (tree2, re2 , states2) = (containment1 effL2 effR delta mode) in
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree1; tree2] ), re2, states1+states2)

  (****If worriy of comokenness, need to delete this part. *****)
  | ( _, Disj (effL1, effL2)) -> 
    (*[RHSOR]*)
      let (tree1, re1, states1 ) = (containment1 normalFormL effL1 delta mode) in
      if re1 == true then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree1] ),  true, states1)
      else 
        let (tree2, re2 , states2) = (containment1 normalFormL effL2 delta mode) in 
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSOR, [tree2] ), re2, states2)
    (****If worriy of comokenness, need to delete this part. *****)


  | (Effect (piL, esL),Effect(piR, ESAnd (esR1, esR2))) ->
      let (tree1, re1, states1 ) = (containment1 normalFormL (Effect(piR, esR1)) delta mode) in
      if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule RHSAND, [tree1] ),  false, states1)
      else
        let (tree2, re2, states2 ) = (containment1 normalFormL (Effect(piR, esR2)) delta mode) in
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule RHSAND, [tree1; tree2] ), re2, states1+states2)


  | (Effect (piL, esL),_) ->
    if checkReoccur normalFormL normalFormR delta then (Node(showEntail ^ "   [Reoccur]", []), true, 0) 
    (*
    else if (entailConstrains (pureUnion normalFormL) (pureUnion normalFormR)) == false then (Node(showEntail ^ "   [Contradictory] "  , []), false, 1) 
    *)
    
    else if (not mode) && (isEmp normalFormR) == true then  (Node(showEntail^"   [Frame-Prove]" ^" with R = "^(showES esL ) , []),true, 0) 
    else if (checkNullable normalFormL) == true && (checkNullable normalFormR) == false then (Node(showEntail ^ "   [REFUTATION] "  , []), false, 0) 
    
    (*
    else if isInfinite esL && notSureInfinite normalFormR then containment1 normalFormL (keepIfinite normalFormR) delta 
      *)
    (*Existential*)

    else if needToBeInstantiated normalFormR (getAllVarFromEff normalFormL) == true then 
    (*if existialRHSEff piL esL normalFormRNew varList == true then*)
      
      let headsofRHS = headEff normalFormR in 
      (*
      print_string (showEffect normalFormR^"\n");
      print_string (string_of_int (List.length headsofRHS)^"\n");
      print_string (List.fold_left (fun acc a  -> acc ^ " " ^ a ^ "\n") ""  varList);
      *)
      let head = getTheheadneedToBeInstantiated headsofRHS varList in
      match head with 
          Ttimes (esIn, term) -> 
          
              (match term with
                Var s -> 
    (*********************************)
    (*                
    3. find possible values 
    4. disjunc all the values in instanstiation
    *)
                let getInstansVal piL esL pattern: int list = 

                  let maxSize = getSize esL in 


                  let rec helper_classic (leftEs:es) (rightEs:es) (acc:int):int= 
                    if acc <= maxSize then 
                      (*print_string (showES leftEs ^"::" ^showES rightEs);*)
                      let ev = simpleFst rightEs in 
                      let leftEs' = normalES (simpleDerivative ev leftEs) TRUE in 
                      let rightEs' = normalES (simpleDerivative ev rightEs) TRUE in 
                      if isBotES leftEs' then acc
                      else if isEmpES rightEs' then helper_classic leftEs' pattern (acc+1) 
                      else helper_classic leftEs' rightEs' (acc+1) 
                    else maxSize
                  in 
                  (*
                  let rec helper (acc:int) (p:es)= 
                    if acc <= maxSize then 
                      let (t, r, s) = containment1 (Effect (piL, esL)) (Effect (TRUE, p)) [] mode in 
                      if r = false then acc
                      else helper (acc+1) (Cons (p, pattern))
                    else maxSize
                  in 
                  print_string ("Existential\n");
                  print_string (string_of_int max^"\n----------------\n");
                  *)

                  let max = helper_classic esL pattern 0 in

                  
                  
                  
                  
                  if itStartsFromANegation normalFormR then List.rev(makeList 1 maxSize [])
                  else List.rev(makeList 1 max []) 
                  
                in 
                let instanceFromLeft = getInstansVal piL esL esIn in 
                let instantiateRHS = instantiateEffR normalFormR instanceFromLeft in 
                (*print_string (List.fold_left (fun acc a  -> acc ^ showEffect a ^ "\n") ""  instantiateRHS);*)
                let rec chceckResultOR li acc staacc : (bool * binary_tree list  * int * int)=
                  (match li with 
                    [] -> (false , acc, staacc, -1) 
                  | (rhs, index)::rhss -> 
                  (*
                      print_string (showEffect rhs^"\n");
                      print_string (string_of_int index^"\n");
                      *)
                      let pure = Eq (Var s, Number index) in 
                      let lhs = normalEffect (addConstrain (Effect (piL, esL)) pure )in 
                      match lhs with 
                        Effect (FALSE, _) -> 
                        chceckResultOR rhss acc staacc
                        
                      | _ -> 
                        let rhs = normalEffect (addConstrain rhs pure) in 
                        let (tree, re, states) = containment1 (lhs) (rhs) delta mode in 
                        if re == true then (true , tree::acc, staacc+states, index)
                        else chceckResultOR rhss (tree::acc) (staacc+states)
                  )
                in 
                let (resultFinal, trees, states, value ) = chceckResultOR instantiateRHS [] 0 in
                if resultFinal then (Node(showEntail ^ "   [EXISTENTIAL "^ s ^ "="^ string_of_int value ^"]", trees ), resultFinal, states) 
                else 
                (Node(showEntail ^ "   [EXISTENTIAL "^ "fail" ^"]", trees ), resultFinal, states) 
                (*********************************)

              | Plus  (Var t, num) -> 
                let newVar = getAfreeVar varList in 
                let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ), GtEq (Var newVar, Number 0)) in
                let lhs' = addConstrain normalFormL cons in
                let rhs' = addConstrain rhs cons in
                let (tree, re, states) = containment1 lhs' rhs' delta mode in
                (Node (showEntailmentEff lhs' rhs' ^ "   [SUB-RHS]",[tree] ), re, states)
                
              | Minus (Var t, num) -> 
                let newVar = getAfreeVar varList in 
                let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ), GtEq (Var newVar, Number 0))in

                let lhs' = addConstrain normalFormL cons in
                let rhs' = addConstrain rhs cons in
                let (tree, re, states) = containment1 lhs' rhs' delta mode in
                (Node (showEntailmentEff lhs' rhs' ^ "   [SUB-RHS]",[tree] ), re, states)
              | _ -> raise (Foo "bu ying gai a ");
              )
        | _ -> raise (Foo "bu ying gai a ");



    else 
(*there is no extantial var on thr RHS already*)
      match hd (headEs esL) with
          Ttimes (esIn, term) -> 
            (match term with 
              Var s -> 
                (match  entailConstrains (Eq (Var s, Number 0) ) piL  with 
                  true -> (*[CASE SPLIT]*) 
                    let zeroCase = PureAnd (piL, Eq (Var s, Number 0) ) in 
                    let nonZeroCase = PureAnd (piL, Gt (Var s, Number 0) ) in 
                    let leftZero = normalEffect (addConstrain (normalFormL) zeroCase) in
                    let rightZero = normalEffect (addConstrain (normalFormR) zeroCase) in
                    let leftNonZero = normalEffect (addConstrain normalFormL nonZeroCase) in
                    let rightNonZero = normalEffect (addConstrain normalFormR nonZeroCase) in


                    (*zhe li hao xiang ke yi gai*)

                    let (tree1, re1, states1) = (containment1 leftZero rightZero delta mode) in
                    (match re1 with 
                      false -> (Node (showEntailmentEff normalFormL normalFormR ^"   [CASE SPLIT]",[tree1] ), re1, states1)
                    | true -> let (tree2, re2, states2) = (containment1 leftNonZero rightNonZero delta mode) in
                      (Node (showEntailmentEff normalFormL normalFormR ^"   [CASE SPLIT]",[tree1;tree2] ), re1&& re2, states1+states2)
                    )
                  | false -> (*[UNFOLD]*)unfold normalFormL (addEntailConstrain normalFormR piL) delta 
                )
            | Plus  (Var t, num) -> 
            (*[LHSSUB]*)
                let newVar = getAfreeVar varList in 
                let lhs = substituteEff normalFormL  (Plus  (Var t, num))  (Var newVar) in
                let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ), GtEq (Var newVar, Number 0)) in
                let lhs' = addConstrain lhs cons in 
                let rhs' = addConstrain rhs cons in 
                let (tree, re, states) = containment1 lhs' rhs' delta mode in
                (Node (showEntailmentEff normalFormL normalFormR ^"   [SUB "^ newVar ^"/" ^ t ^"+1]",[tree] ), re, states)
            | Minus (Var t, num) -> 
            (*[LHSSUB]*)
                let newVar = getAfreeVar varList in 
                let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ), GtEq (Var newVar, Number 0))in
                let lhs' = addConstrain lhs cons in 
                let rhs' = addConstrain rhs cons in 
                let (tree, re, states) = containment1 lhs' rhs' delta mode in
                (Node (showEntailmentEff normalFormL normalFormR ^"   [SUB "^ newVar ^"/" ^ t ^"-1]",[tree] ), re, states)
            | Number n -> 
            
            unfold normalFormL (addEntailConstrain normalFormR piL) delta 
            | _ -> print_endline (showEntailmentEff normalFormL normalFormR);
              raise ( Foo "term is too complicated exception1!")
            )
          | _ ->  unfold normalFormL (addEntailConstrain normalFormR (piL)) delta 
  ;;

(*
let rec containment (effL:effect) (effR:effect) (delta:context) (varList:string list): (binary_tree * bool * int) = 
  let normalFormL = normalEffect effL in 
  let normalFormR = normalEffect effR in
  let showEntail  = (*showEntailmentEff effL effR ^ " ->>>> " ^*)showEntailmentEff normalFormL normalFormR in 
  (*
  print_string(showEntail ^"\n");
  *)
  let unfoldSingle ev piL esL piR esR (del:context) = 
    let derivL = derivative piL esL ev in
    let derivR = derivative piR esR ev in
    let (tree, result, states) = containment derivL derivR del varList in
    (Node (showEntailmentEff ( (Effect(piL, esL))) ((Effect(piR, esR))) ^ "   [Unfold with Fst = "^  ev ^ "]",[tree] ), result, states+1)
  in
  (*Unfold function which calls unfoldSingle*)
  let unfold del piL esL piR esR= 
    let fstL = remove_dups (fst piL esL )in 
    (*
    let hypos = getNewHypos fstL piL esL piR esR in 
    *)

    let deltaNew = append del [(piL, esL, piR, esR)]in
    let rec chceckResultAND li acc staacc:(bool *binary_tree list* int )=
      (match li with 
        [] -> (true, acc, staacc) 
      | ev::fs -> 
          let (tree, re, states) = unfoldSingle ev piL esL piR esR deltaNew in 
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
        (Node (showEntailmentEff normalFormL normalFormR ^ showRule RHSOR, [tree1; tree2] ), re2, states1+states2+1)
  | (Effect (piL, esL), Effect (piR, esR))-> 
    let lhs' = remove_dup (splitCons esL) in 
    let rhs' = remove_dup (splitCons esR) in 
    let esL =  (connectDisj lhs') in 
    let esR =  (connectDisj rhs') in 
      if entailConstrains piL piR == false then (Node(showEntail ^ "   [Contradictory]", []), false, 0)  
      else 
      (*Existential*)
        if existialRHS piL esL esR varList == true then
          let instanceFromLeft = getInstansVal piL esL in 
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
      else if (comparePure piR FALSE == true ) then (Node(showEntail ^ "   [DISPROVE] "  , []), false, 1)
      (*[REFUTATION]*)
      else if (nullable piL esL) == true && (nullable piR esR) == false 
        then (Node(showEntail ^ "   [REFUTATION] "  , []), false, 1) 
      (*[Frame]*)
      else if (isEmp normalFormR) == true  
        then  (Node(showEntail^"   [Frame-Prove]" ^" with R = "^(showES esL ) , []),true, 1) 
      (*[Reoccur]*)
      (*
        else if (reoccurCtxSet (fromListToSet lhs') (fromListToSet rhs') delta) == true 
        *)
      else if (reoccurHelp piL esL piR esR  delta) == true 
        then 
        (
          (*print_string ("\n"^showEntail^"\n");*)
          (Node(showEntail ^ "   [Reoccur-Prove] "  , []), true, 0) 
        )
      (*Transitivity
        else if (transitivity piL esL piR esR delta )== true 
        then (Node(showEntail ^ "   [Reoccur-Transitive] "  , []), true, 0) 
      *)
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
                (match  entailConstrains (Eq (Var s, Number 0) ) piL  with 
                  true -> (*[CASE SPLIT]*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, Number 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, Number 0) ) in 
                            let leftZero = addConstrain (Effect(piL, Emp)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSCASE ^ " *Pruning search*",[tree1] ), re1, states1)
                            else
                            let (tree2, re2 , states2) = (containment leftNonZero rightNonZero delta varList) in
                            (Node (showEntailmentEff normalFormL normalFormR ,[tree1; tree2] ), re1 && re2, states1+states2+1)
                | false -> (*[UNFOLD]*)unfold delta piL esL piR esR
                )
            | Plus  (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num))  (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ), PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
            | Minus (Var t, num) -> 
            (*[LHSSUB]*)
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ),PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) ) )in
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
                (match  entailConstrains (Eq (Var s, Number 0) ) piL with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, Number 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, Number 0) ) in 
                            let leftZero = addConstrain (Effect(piL, restES)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1 , states1) = (containment leftZero rightZero delta varList) in
                            if re1 == false then (Node (showEntailmentEff normalFormL normalFormR ^ showRule LHSCASE ^ " *Pruning search*",[tree1] ), re1, states1)
                            else 
                            let (tree2, re2, states2 ) = (containment leftNonZero rightNonZero delta varList) in
                            (Node (showEntailmentEff normalFormL normalFormR ,[tree1; tree2] ), re1 && re2, states1+states2+1)
                        | false -> (*UNFOLD*) unfold delta piL esL piR esR
                        )
              | Plus  (Var t, num) -> 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ), PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
                        let lhs' = addConstrain lhs cons in 
                        let rhs' = addConstrain rhs cons in 
                        let (tree, re, states) = containment lhs' rhs' delta (newVar::varList)in
                        (Node (showEntailmentEff normalFormL normalFormR ,[tree] ), re, states)
              | Minus (Var t, num) -> 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ), PureOr (Gt (Var newVar,Number  0) , Eq (Var newVar, Number 0) )) in
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
                        (match  entailConstrains (Eq (Var s, Number 0) ) piL with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, Number 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, Number 0) ) in 
                            let leftZero = addConstrain normalFormL zeroCase in
                            let rightZero = addConstrain (Effect(piR, Emp)) zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == true then (Node (showEntailmentEff effL effR ,[tree1] ), true, states1) 
                            else 
                              let (tree2, re2, states2 ) = (containment leftNonZero rightNonZero delta varList) in
                              (Node (showEntailmentEff effL effR ,[tree1; tree2] ), re2, states1+states2+1)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ),PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
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
                        let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ), PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
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
                        (match  entailConstrains (Eq (Var s, Number 0) ) piL with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piR, Eq (Var s, Number 0) ) in 
                            let nonZeroCase = PureAnd (piR, Gt (Var s, Number 0) ) in 
                            let leftZero = addConstrain normalFormL zeroCase in
                            let rightZero = addConstrain (Effect(piR, restESR)) zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            let (tree1, re1, states1 ) = (containment leftZero rightZero delta varList) in
                            if re1 == true then (Node (showEntailmentEff normalFormL normalFormR , [tree1] ), true, states1)
                            else 
                            let (tree2, re2, states2 ) =  (containment leftNonZero rightNonZero delta varList) in 
                            (Node (showEntailmentEff normalFormL normalFormR , [tree1; tree2] ), re2, states1+states2+1)
                        | false -> (*UNFOLD*)unfold delta piL esL piR esR
                        )
                | Plus  (Var t, num) -> 
                        if quantified_in_LHS esL t then unfold delta piL esL piR esR
                        else 
                        let newVar = getAfreeVar varList in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar)  in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num))  (Var newVar) in
                        let cons = PureAnd( Eq (Var newVar, Plus (Var t, num) ), PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
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
                        let cons = PureAnd( Eq (Var newVar, Minus (Var t, num) ), PureOr (Gt (Var newVar, Number 0) , Eq (Var newVar, Number 0) )) in
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
  *)
(*----------------------------------------------------
----------------------TESTING-------------------------
----------------------------------------------------*)

type expectation = bool

type entailment =  (effect * effect * expectation) 



(*
let ttest = (Plus ((Var "song"),Number 1));;
let ttest1 = (Var "t");;
let estest = ESOr (Cons (Ttimes ((Event ("a",None)), Var "t"),  (Event ("a",None))), Cons ((Event ("a",None)),(Event ("b",None))));;
let puretest =  Eq (ttest1, Number 0);;
let testes = Effect (puretest, estest, []);; 
let testcontext =  [testes; testes];;
let testD = derivative puretest estest ("a",None);;
let leftEff = Effect (TRUE, ESOr (Omega (Event ("a",None)), Omega (Event ("b",None)))) ;;
let rightEff = Effect (TRUE, Omega (Event ("b",None))) ;;
let leftEff1 = Effect (TRUE, Cons (Event ("a",None), Cons (Event ("b",None), Event ("c",None)))) ;;
let rightEff2 = Effect (TRUE, Cons (Event ("a",None), Cons (Event ("d",None), Event ("c",None)))) ;;
let lhsss = Effect (TRUE, Cons (Ttimes ((Event ("a",None)), Var "t"), Event ("c",None)));;
let rhsss = Effect (TRUE, Omega ((Event ("a",None))));;




(*Printf.printf "%s" (showTerms  ttest);;
Printf.printf "%s" (showES estest);;

Printf.printf "%s" (showPure puretest);;

Printf.printf "%s" (showEffect testes);;
Printf.printf "%s" (showContext testcontext );;*)

let a = Event ("Tick",None) ;;
let b = Event ("b",None) ;;
let c = Event ("c",None) ;;
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

let createT_1 es = Ttimes (es, Minus (Var "t", Number 1) );;

let createS_1 es = Ttimes (es, Minus (Var "s", Number 1) );;


*)
let printReportHelper lhs rhs (mode:bool): (binary_tree * bool * int) = 
  (*
  let delta = getProductHypo lhs rhs in 
    let varList = append (getAllVarFromEff lhs) (getAllVarFromEff rhs) in  
  let varList = getAllVarFromEff lhs in  

  *)

  containment1 lhs rhs [] mode
  ;;



let printReport lhs rhs (mode:bool):string =
  let startTimeStamp = Sys.time() in
  let (tree, re, states) =  printReportHelper lhs rhs mode in
  let verification_time = "[Verification Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in
  let result = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  let states = "[Explored "^ string_of_int (states) ^ " States]\n" in 
  let buffur = ( "===================================="^"\n" ^(showEntailmentEff lhs rhs)^"\n[Result] " ^(if re then "Succeed\n" else "Fail\n") ^ states ^verification_time^" \n\n"^ result)
  in buffur
  ;;
(*
let testcases : entailment list= 
  [

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
  ,Effect(Gt(Var "t", Number 0), createT a)
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

  (Effect(Gt(Var "t", Number 0), createT a)
  ,Effect(TRUE, createT_1 a)
  ,true
  )
  ;
   (*THIS ONE IS WRONG!*)
  (Effect(Gt(Var "s", Number 0), Cons (createT a ,createS b))
  ,Effect(TRUE, Cons (createT a ,createS_1 b))
  ,true
  )
  ;
  (Effect(TRUE, omegaA)
  ,Effect(TRUE, createT_1 a)
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
*)
