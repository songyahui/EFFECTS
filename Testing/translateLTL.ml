open Random
open Ast
open Pretty
open Printf
open List
open String
open Rewriting



let testASingle pi ff ss :string = 
  print_string (ff ^"  "^ss^"\n");
  let lhs = addConstrain (Effect(TRUE, (Parser.es_p Lexer.token (Lexing.from_string ff)))) pi  in
  let rhs = addConstrain (Effect(TRUE, (Parser.es_p Lexer.token (Lexing.from_string ss)))) pi  in
  (*let rhs:(es) = Parser.es_p Lexer.token (Lexing.from_string  ss)  in*)
  printReport lhs rhs true
  (*
  let helper li acc = 
    match li with 
      [] -> acc 
    | EE(lhs, rhs) ::xs -> 
    acc ^ printReport lhs rhs
  in 
    helper eelist ""
*)
  ;;


let main = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let specs:(string list) =  (input_lines ic) in 
    let lines = List.fold_right (fun x acc -> acc ^ "\n" ^ x) ( specs) "" in 
    let ltlList:(ltl list) = Parser.ltl_p Lexer.token (Lexing.from_string  lines)  in
(*
    let ential_result=List.fold_right (fun ltl acc->  
    let (pi, esss, varL) =translateLTL TRUE ltl [] in 
    let rhs = "|- "^ showES esss ^ ";" in
    acc ^ testASingle pi trafic rhs
    ) (ltlList) "" in 
    print_string (ential_result^"\n");
    *)
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    let esList = List.map (fun ltl -> 
      let (a, b, c) = (translateLTL TRUE ltl []) in 
      Effect (a, b)   ) ltlList in
    (*
    print_string (showLTLList ^ "\n==============\n");
    *)
    
    let producte = List.combine ltlList esList in

    let result = List.fold_right (fun (l,e) acc -> acc ^ "  "^(*showLTL l ^ " ==> "^*)(showEffect e) ^";.\n") (producte)  "" in 
    
    print_string (result^"\n");
    
    
    
    
    fprintf oc "%s\n" result;   (* 写一些东西 *)


    close_out oc;                (* 写入并关闭通道 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;

  