open Random
open Ast
open Pretty
open Printf
open List
open Rewriting





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


let main = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
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
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
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
    
    
    fprintf oc "%s\n" result;   (* 写一些东西 *)


    close_out oc;                (* 写入并关闭通道 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;

  