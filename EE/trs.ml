open Rewriting
open Printf

let () = 
  print_string ("test");
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let line = String.trim (input_line ic ) in  (* 从输入通道读入一行并丢弃'\n'字符 *)
    let EE (lhs, rhs) = Parser.ee Lexer.token (Lexing.from_string line) in
    let result = printReport lhs rhs in 
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s\n" result;   (* 写一些东西 *)
    close_out oc;                (* 写入并关闭通道 *)
    print_string result;
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;