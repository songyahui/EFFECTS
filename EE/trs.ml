open Rewriting
open Printf
open List 
open Ast

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

let () = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let lines =  (input_lines ic ) in  
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (lines) "" in 
    let eeList = Parser.ee Lexer.token (Lexing.from_string line) in
    let result = List.map (fun parm ->  
                            match parm with 
                              EE (lhs, rhs) -> printReport lhs rhs ) eeList in 
    let final_result = List.fold_right (fun x acc -> acc  ^ x ^ "\n") ( result) "" in 
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s\n" final_result;   (* 写一些东西 *)
    close_out oc;                (* 写入并关闭通道 *)
    (*
    print_string final_result;
    *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;