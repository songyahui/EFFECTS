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



(*used to generate the free veriables, for subsititution*)
let freeVar = ["t1"; "t2"; "t3"; "t4";"t5";"t6";"t7";"t8";"t9";"t10"
              ;"t11"; "t12"; "t13"; "t14";"t15";"t16";"t17";"t18";"t19";"t20"
              ;"t21"; "t22"; "t23"; "t24";"t25";"t26";"t27";"t28";"t29";"t30"];;



let rec getAfreeVar (varList:string list):string  =
  let rec findOne li = 
    match li with 
        [] -> raise ( Foo "freeVar list too small exception!")
      | x :: xs -> if (exists (fun a -> String.compare a x == 0) varList) == true then findOne xs else x
  in
  findOne freeVar
;;

(*
let rec translateLTL (pi:pure) (ltl:ltl) (varList:string list) :(pure * es * string list) =
  match ltl with 
    Lable str -> (pi, Event (str, None), varList)
  | Next l -> 
    let (piii, ess, varList') =  translateLTL pi l varList in 
    (piii,  Cons (Underline,ess), varList')
  | Until (l1, l2) -> 
      let newVar = getAfreeVar varList in 
      let newPi = PureAnd (pi, Gt (Var newVar, Number 0)) in 
      let (pi1, ess1, varList1) =  translateLTL newPi l1 (newVar :: varList) in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      let prefix = Ttimes (ess1, Var newVar) in 
      (*(pi2, Cons (Cons(ess1, Kleene (ess1)) , ess2), varList2)*)
      (pi2, Cons (prefix, ess2), varList2)
  | Global l -> 
      let (piii , ess1, varList') =  translateLTL pi l varList in 

      (piii, Kleene (ess1), varList')
  | Future l -> 
      let newVar = getAfreeVar varList in 
      let prefix = Ttimes (Underline, Var newVar) in 
      let (piii, ess, varList') =  translateLTL pi l (newVar::varList) in 
 
      (*(piii, Cons (Kleene(Not ess ), ess), varList')*)
      (piii, Cons (prefix, ess), varList')
  | NotLTL l -> 
      let (piii, ess, varList') =  translateLTL pi l varList in 
      (piii, Not (ess), varList')
  | Imply (l1, l2) -> 
      let (pi1, ess1, varList1) =  translateLTL pi l1 varList in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      (pi2, ESOr ( (Not (ess1)),   ess2), varList2)
  | AndLTL (l1, l2) -> 
      let (pi1, ess1, varList1) =  translateLTL pi l1 varList in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      (pi2, ESAnd (ess1, ess2), varList2)
  | OrLTL (l1, l2) -> 
      let (pi1, ess1, varList1) =  translateLTL pi l1 varList in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      (pi2, ESOr (ess1, ess2), varList2)
  ;;
*)



let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

  ;;

let get_0 (a,_) = a ;;

let get_1 (_, a,_) = a ;;


let rec showLTL (ltl:ltl):string =
  match ltl with 
    Lable str -> str
  | Next l -> "(" ^"X" ^showLTL l ^")"
  | Until (l1, l2) -> "(" ^showLTL l1 ^ " U " ^showLTL l2 ^")"
  | Global l -> "(" ^"[] " ^showLTL l ^")"
  | Future l -> "(" ^"<> " ^showLTL l ^")"
  | NotLTL l -> "(" ^"! " ^showLTL l ^")"
  | Imply (l1, l2) -> "(" ^showLTL l1 ^ " -> " ^showLTL l2 ^")"
  | AndLTL (l1, l2) -> "(" ^showLTL l1 ^ " && " ^showLTL l2 ^")"
  | OrLTL (l1, l2) -> "(" ^showLTL l1 ^ " || " ^showLTL l2 ^")"
  ;;

let compareParm (p1:int option ) (p2:int option ) :bool = 
  match (p1, p2) with 
    (None, None) -> true 
  | (Some n1, Some n2) -> n1 == n2
  | _ -> false 
  ;;

let compareEvent (ev1:(string* int option)) (ev2:(string* int option)):bool =
  let (str1, p1) = ev1 in
  let (str2, p2) = ev2 in
  String.compare str1 str2 == 0 && compareParm p1 p2
  ;;

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
          | RHSAND

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
  | Event (ev, None) -> ev  
  | Event (ev, Some num) -> ev ^"("^string_of_int num^")"
  | Not (ev, None) ->  "!(" ^ (ev) ^")"
  | Not (ev, Some num) -> "!(" ^ ev ^"("^string_of_int num^")" ^")"
  | Cons (es1, es2) -> "("^(showES es1) ^ " . " ^ (showES es2)^")"
  | ESOr (es1, es2) -> "("^(showES es1) ^ "|" ^ (showES es2)^")"
  | ESAnd (es1, es2) -> "("^(showES es1) ^ "/\\" ^ (showES es2)^")"
  | Ttimes (es, t) -> "("^(showES es) ^ "^" ^ (showTerms t)^")"
  | Underline -> "_"
  | Kleene es -> "(" ^ (showES es) ^ "^" ^ "*"^")"
  ;;







(*To pretty print pure formulea*)
let rec showPure (p:pure):string = 
  match p with
    TRUE -> "true"
  | FALSE -> "false"
  | Gt (t1, t2) -> (showTerms t1) ^ ">" ^ (showTerms t2)
  | Lt (t1, t2) -> (showTerms t1) ^ "<" ^ (showTerms t2)
  | GtEq (t1, t2) -> (showTerms t1) ^ ">=" ^ (showTerms t2)
  | LtEq (t1, t2) -> (showTerms t1) ^ "<=" ^ (showTerms t2)
  | Eq (t1, t2) -> (showTerms t1) ^ "=" ^ (showTerms t2)
  | PureOr (p1, p2) -> "("^showPure p1 ^ "\\/" ^ showPure p2^")"
  | PureAnd (p1, p2) -> "("^showPure p1 ^ "/\\" ^ showPure p2^")"
  | Neg p -> "(!" ^ "(" ^ showPure p^"))"
  ;; 

(*To pretty print effects*)
let rec showEffect (e:effect) :string = 
  match e with
    Effect (p, es) -> 
      showPure p ^ "&" ^ showES es 
  | Disj (es1, es2) ->  showEffect es1 ^ " \\/ "  ^ showEffect es2 
  ;;

(*To pretty print effects entialments*)


let showEntailmentEff (eff1:effect)( eff2:effect):string = showEffect eff1 ^ " |- "  ^ showEffect eff2;;

(*To pretty print event sequence entailment*)
let showEntailmentES (es1:es) (es2:es):string = showES es1 ^ " |- "  ^ showES es2;;


(*To pretty print entialment rules*)
let showRule (r:rule):string = 
  match r with
    LHSOR -> " [LHSOR] "
  | RHSAND -> " [RHSAND] "
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

(*To pretty print all the context entailments
let rec showContext (d:context):string = 
  match d with
    [] -> ""
  | (piL, esL, piR, esR)::rest -> (showEntailmentEff (Effect (piL, esL)) (Effect (piR, esR)) )^ ("\n") ^ showContext rest
  ;;
  *)

let rec reverseEs (es:es) : es = 
  match es with 
    Bot -> Bot
  | Emp -> Emp
  | Event _ -> es
  | Not _ ->  es
  | Cons (es1, es2) -> Cons (reverseEs es2, reverseEs es1)
  | ESOr (es1, es2) -> ESOr (reverseEs es1, reverseEs es2)
  | ESAnd (es1, es2) -> ESAnd (reverseEs es1, reverseEs es2)
  | Ttimes (es1, t) -> Ttimes (reverseEs es1, t)
  | Underline -> Underline
  | Kleene es1 ->  Kleene (reverseEs es1)
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
  | Event _  -> es
  | Not _  -> es
  | Cons (es1, es2) ->  Cons (substituteESWithAgr es1 realArg formalArg, substituteESWithAgr es2 realArg formalArg)
  | ESOr (es1, es2) ->  ESOr (substituteESWithAgr es1 realArg formalArg, substituteESWithAgr es2 realArg formalArg)
  | ESAnd (es1, es2) ->  ESAnd (substituteESWithAgr es1 realArg formalArg, substituteESWithAgr es2 realArg formalArg)
  | Ttimes (esIn, t) -> Ttimes (substituteESWithAgr esIn realArg formalArg, substituteTermWithAgr t realArg formalArg)
  | Kleene esIn -> Kleene (substituteESWithAgr esIn realArg formalArg)
  | Underline -> es
  ;;


let rec splitDisj (p:pure) (es:es):effect =
  match p with 
    PureOr (p1, p2) -> Disj (splitDisj p1 es , splitDisj p2 es ) 
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
  (*let rec subESsetOf (small : es list) (big : es list) :bool = 
    let rec oneOf a set :bool = 
      match set with 
        [] -> false 
      | y:: ys -> if aCompareES a y then true else oneOf a ys
    in 
    match small with 
      [] -> true 
    | x :: xs -> if oneOf x big == false then false else subESsetOf xs big
  in 
  *)

  match (es1, es2) with 
    (Bot, Bot) -> true
  | (Emp, Emp) -> true
  | (Event (s1, p1), Event (s2,p2)) -> 
    String.compare s1 s2 == 0 && compareParm p1 p2 
  | (Not (s1, p1), Not (s2,p2)) -> 
    String.compare s1 s2 == 0 && compareParm p1 p2 
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> 
    if (aCompareES es1L es2L) == false then false
    else (aCompareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      if ((aCompareES es1L es2L) && (aCompareES es1R es2R)) then true 
      else ((aCompareES es1L es2R) && (aCompareES es1R es2L))
  | (ESAnd (es1L, es1R), ESAnd (es2L, es2R)) -> 
      if ((aCompareES es1L es2L) && (aCompareES es1R es2R)) then true 
      else ((aCompareES es1L es2R) && (aCompareES es1R es2L))
  | (Kleene esL, Kleene esR) -> aCompareES esL esR
  | _ -> false
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



