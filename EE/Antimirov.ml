open Ast
open List
open Pretty


exception Foo of string

type evn = (es*es) list

let rec aCompareES es1 es2 = 
  match (es1, es2) with 
    (Bot, Bot) -> true
  | (Emp, Emp) -> true
  | (Event s1, Event s2) -> 
    String.compare s1 s2 == 0
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> (aCompareES es1L es2L) && (aCompareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      let one = ((aCompareES es1L es2L) && (aCompareES es1R es2R)) in
      let two =  ((aCompareES es1L es2R) && (aCompareES es1R es2L)) in 
      one || two
  | (Kleene esL, Kleene esR) -> aCompareES esL esR
  | _ -> false
;;

let rec aNullable (es:es) : bool=
  match es with
    Emp -> true
  | Bot -> false 
  | Event ev -> false 
  | Cons (es1 , es2) -> (aNullable es1) && (aNullable es2)
  | ESOr (es1 , es2) -> (aNullable es1) || (aNullable es2)
  | Kleene es1 -> true
  | _ -> raise (Foo "aNullable exeption\n")
;;

let rec aFst (es:es): event list = 
  match es with
    Emp -> []
  | Event ev ->  [ev]
  | Cons (es1 , es2) ->  if aNullable es1 then append (aFst es1) (aFst es2) else aFst es1
  | ESOr (es1, es2) -> append (aFst es1) (aFst es2)
  | Kleene es1 -> aFst es1
  | _ -> raise (Foo "aFst exeption\n")
;;

let isBot (es:es) :bool= 
  match es with
    Bot -> true
  | _ -> false 
  ;;

let rec remove_dup lst= 
  match lst with
      | [] -> []
      | h::t -> h::(remove_dup (List.filter (fun x -> x<>h) t))
      ;;



let rec splitCons (es:es) : es list = 
  match es with 
    ESOr (es1, es2) -> append (splitCons es1) (splitCons es2)
  | _ -> [es]
  ;;


let rec aReoccur esL esR (del:evn) = 
  match del with 
  | [] -> false 
  | (es1, es2) :: rest -> 
    let tempH = splitCons es2 in 
    let temp = splitCons esR in 
    let superset = List.fold_left (fun acc a -> acc && List.mem a temp  ) true tempH in
    if (aCompareES esL es1 && superset) then true
    else aReoccur esL esR rest (*REOCCUR*) 
  ;;


let rec aDerivative (es:es) (ev:string): es =
  match es with
    Emp -> Bot
  | Bot -> Bot
  | Event ev1 -> 
      if (String.compare ev1 ev) == 0 then Emp else Bot
  | ESOr (es1 , es2) -> ESOr (aDerivative es1 ev, aDerivative es2 ev)
  | Cons (es1 , es2) -> 
      if aNullable es1 
      then let efF = aDerivative es1 ev in 
          let effL = Cons (efF, es2) in 
          let effR = aDerivative es2 ev in 
          ESOr (effL, effR)
      else let efF = aDerivative es1 ev in 
          Cons (efF, es2)    
  | Kleene es1 -> Cons  (aDerivative es1 ev, es)
  | _ -> raise (Foo "antimirovDerivative exeption\n")

;;

let rec aNormalES es:es  =
(*
    let rec antimirovLight (lhs:es) (rhs:es) (evn:evn ): bool = 
    let normalFormL = aNormalES lhs in 
    let normalFormR = aNormalES rhs in
    let unfoldSingle ev esL esR (del:evn) = 
      let derivL = aDerivative esL ev in
      let derivR = aDerivative esR ev in
      let (result) = antimirovLight derivL derivR del in
      result
    in
    (*Unfold function which calls unfoldSingle*)
    let unfold del esL esR= 
      let fstL = remove_dup (aFst esL )in 
      (*print_string ("\n" ^List.fold_left (fun acc a -> acc ^ "-"^ a) "" fstL^"\n");*)
      let deltaNew:(evn) = append del [(esL, esR)] in
      let rec chceckResultAND li:bool=
        (match li with 
          [] -> (true) 
        | ev::fs -> 
            let (re) = unfoldSingle ev esL esR deltaNew in 
            if re == false then (false)
            else chceckResultAND fs 
        )
      in 
      let (resultFinal) = chceckResultAND fstL  in 
      (resultFinal)    
    
    in 
    if (isBot normalFormR) then false
    (*[REFUTATION]*)
    else if (aNullable normalFormL) == true && (aNullable normalFormR) == false then false
        (*[Reoccur]*)
    else if (aReoccur normalFormL normalFormR evn) == true then true
        (*Unfold*)                    
    else 
    match (normalFormL, normalFormR) with
      (ESOr (effL1, effL2), _) -> 
      (*[LHSOR]*)
        let (re1 ) = (antimirovLight effL1 normalFormR evn) in
        if re1 == false then (false)
        else 
          let (re2) = (antimirovLight effL2 normalFormR evn) in
          (re2)
    | _ -> 
    unfold evn normalFormL normalFormR
  in 
*)
  match es with
    Bot -> es
  | Emp -> es
  | Event ev -> es
  | Cons (Cons (esIn1, esIn2), es2)-> aNormalES (Cons (esIn1, Cons (esIn2, es2))) 
  | Cons (es1, es2) -> 
      let normalES1 = aNormalES es1 in
      let normalES2 = aNormalES es2 in
      (match (normalES1, normalES2) with 
        (Emp, _) -> normalES2
      | (_, Emp) -> normalES1
      | (Bot, _) -> Bot
      | (Omega _, _ ) -> normalES1
      (*| (ESOr (or1, or2), es2) -> ESOr (aNormalES (Cons (or1, es2)), aNormalES (Cons (or2, es2))) 
      | (es1, ESOr (or1, or2)) -> ESOr (aNormalES (Cons (es1, or1)), aNormalES (Cons (es1, or2))) 
      *)
      | (Kleene (esIn1), Kleene (esIn2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2)
      | (Kleene (esIn1), Cons(Kleene (esIn2), es2)) -> 
          if aCompareES esIn1 esIn2 == true then normalES2
          else Cons (normalES1, normalES2)
      | (normal_es1, normal_es2) -> Cons (normal_es1, normal_es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (aNormalES es1, aNormalES es2) with 
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
  | Kleene es1 -> 
      let normalInside = aNormalES es1 in 
      (match normalInside with
        Emp -> Emp
      | Kleene esIn1 ->  Kleene (aNormalES esIn1)
      | ESOr(Emp, aa) -> Kleene aa
      | _ ->  Kleene normalInside)
  | _ -> raise (Foo "antimirovNormalES exeption\n")
  ;;

(*
let rec aReoccur esL esR (del:evn) = 
  match del with 
  | [] -> false 
  | (es1, es2) :: rest -> 
    if (aCompareES esL es1 && aCompareES esR  es2) then true
    else aReoccur esL esR rest (*REOCCUR*) 
  ;;
*)

let rec antimirov (lhs:es) (rhs:es) (evn:evn ): (bool * int) = 
  (*
  if (List.length evn >500) then (false, 500) 
  else 
  *)
  let normalFormL = aNormalES lhs in 
  let normalFormR = aNormalES rhs in
  
  
  
  let unfoldSingle ev esL esR (del:evn) = 
    let derivL = aDerivative esL ev in
    let derivR = aDerivative esR ev in
    let (result, states) = antimirov derivL derivR del in
    (result, states+1)
  in
  (*Unfold function which calls unfoldSingle*)
  let unfold del esL esR= 
    let fstL = remove_dup (aFst esL )in 
    (*print_string ("\n" ^List.fold_left (fun acc a -> acc ^ "-"^ a) "" fstL^"\n");*)
    let deltaNew:(evn) = append del [(esL, esR)] in
    let rec chceckResultAND li staacc:(bool * int )=
      (match li with 
        [] -> (true, staacc) 
      | ev::fs -> 
          let (re, states) = unfoldSingle ev esL esR deltaNew in 
          if re == false then (false , staacc+states)
          else chceckResultAND fs (staacc+states)
      )
    in 
    let (resultFinal, states) = chceckResultAND fstL  0 in 
    (resultFinal, states)    
  
  in 
  if (isBot normalFormR) then 
  (
    (*print_string ("here1\n");*)
  (false, 1)
  )
  (*[REFUTATION]*)
  else if (aNullable normalFormL) == true && (aNullable normalFormR) == false then 
  (
    (*print_string ("here2\n");*)
    ( false, 1) 
  )
      (*[Reoccur]*)
  else if (aReoccur normalFormL normalFormR evn) == true then ( true, 1) 
      (*Unfold*)                    
  else 
  match (normalFormL, normalFormR) with
    (ESOr (effL1, effL2), _) -> 
    (*[LHSOR]*)
      let (re1, states1 ) = (antimirov effL1 normalFormR evn) in
      if re1 == false then (false, states1)
      else 
        let (re2 , states2) = (antimirov effL2 normalFormR evn) in
        (re2, states1+states2+1)
  (*| (_, ESOr (effR1, effR2)) -> 
  (*[RHSOR]*)
    let (re1, states1 ) = (antimirov normalFormL effR1 evn) in
    if re1 == true then ( true, states1)
    else 
      let (re2 , states2) = (antimirov normalFormL effR2 evn) in
      (re2, states1+states2)
      *)
  | _ -> 
  (*
  let showEntail  = (*showEntailmentEff effL effR ^ " ->>>> " ^*)showEntailmentES normalFormL normalFormR in 
  print_string (showEntail^"\n\n");
  *)
  unfold evn normalFormL normalFormR
  ;;


let antimirov_shell (lhs:string) (rhs:string) : (bool * int) = 
  

  
  let es1 = Parser.es_p Lexer.token (Lexing.from_string lhs) in
  let es2 = Parser.es_p Lexer.token (Lexing.from_string rhs) in
  antimirov es1 es2 [] 
  ;;
