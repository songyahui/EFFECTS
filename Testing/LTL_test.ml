open Random
open Ast
open Pretty
open Printf
open List
open Rewriting
open Antimirov 





let rec getAfreeVar (varList:string list):string  =
  let rec findOne li = 
    match li with 
        [] -> raise ( Foo "freeVar list too small exception!")
      | x :: xs -> if (exists (fun a -> String.compare a x == 0) varList) == true then findOne xs else x
  in
  findOne freeVar
;;




let checkSingle (lhs:es) (pi:pure) (esR:es) : string = 

  printReport (Effect(pi, lhs)) (Effect(pi, esR)) false

  ;;


(* 
here is the paper. 
https://pat.comp.nus.edu.sg/wp-source/resources/public/pdf/PATManual.pdf 
*)

(*
(* this is for || *)
let rec interleveParalle  es1 es2 acc: es = 
  1. find the fist sync event 
  2. Cons (interleaving LEFT-SYNC,  sync event  , interleveParalle LEFT-SYNC)


  ;;

  *)

(* this is for ||| *)
let rec interleaving  es1 es2 acc: es = 
  let es1 = aNormalES es1 in 
  let es2 = aNormalES es2 in 
  match (es1, es2) with
    (Emp , Emp) -> acc
  | (Emp, es2_p) -> Cons (acc, es2_p) 
  | (es1_p, Emp) -> Cons (acc, es1_p)
  | (Kleene es1_p, Kleene es2_p) -> Kleene (interleaving es1_p es2_p acc)
  | (Kleene es1_p, es2_p)-> Cons (Cons (es1, interleaving es1_p es2_p acc ) , es1)
  | (es1_p,Kleene es2_p) ->Cons (Cons (es2, interleaving es1_p es2_p acc ) , es2)
  | _ -> 

    
  let fstSetES1 = aFst es1 in 
  let fstSetES2 = aFst es2 in

  let esLfromes1 = (List.fold_left (fun accin (esa , n) -> 
    let derES1 = aDerivative es1 (esa , n)  in 
    let rest = interleaving derES1 es2 (Cons (acc, Event (esa , n))) in 
    ESOr (accin, rest)
  ) Bot  fstSetES1) 
  in 
  let esLfromes2 = (List.fold_left (fun accin (esa , n) -> 
    let derES2 = aDerivative es2 (esa , n)  in 
    let rest = interleaving es1 derES2 (Cons (acc, Event (esa , n))) in 
    ESOr (accin, rest)
  ) Bot  fstSetES2) 
  in 
  ESOr (esLfromes1, esLfromes2)

;;


let main = 
 (* let str1 = "((A . C))" in 
  let str2 = "((B . D))" in 
  let es1 = Parser.es_p Lexer.token (Lexing.from_string  str1) in 
  let es2 = Parser.es_p Lexer.token (Lexing.from_string  str2) in 
  let result = interleaving es1 es2 Emp in 
  print_string  (showES es1 ^ " ||| " ^ showES es2 ^ " => \n" ^ showES (aNormalES  result) ^"\n");
  *)
  
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let ic = open_in inputfile in
  try 
    let specs:(string list) =  List.rev(input_lines ic) in 
    let lines = List.fold_right (fun x acc -> acc ^ "\n" ^ x) ( specs) "" in 
    let (csp, ltlList):(es*ltl list) = Parser.ltl_verify Lexer.token (Lexing.from_string  lines)  in
(*
    let ential_result=List.fold_right (fun ltl acc->  
    let (pi, esss, varL) =translateLTL TRUE ltl [] in 
    let rhs = "|- "^ showES esss ^ ";" in
    acc ^ testASingle pi trafic rhs
    ) (ltlList) "" in 
    print_string (ential_result^"\n");
    *)
    let effect_List = List.map (fun ltl -> 
      let (a, b, c) = (translateLTL TRUE ltl []) in 
      (a, b)   ) (List.rev ltlList) in
    (*
    print_string (showLTLList ^ "\n==============\n");
    *)
    


    let result = List.fold_right (fun (a, b) acc -> acc ^ checkSingle csp a b ^"\n\n") (effect_List)  "" in 
    (*
    print_string (result^"\n");
    
    *)
    
    
    print_string (result);


    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;

  