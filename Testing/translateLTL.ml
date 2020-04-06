open Random
open Ast
open Pretty
open Printf
open List

let test = "[]<>green;\n"^ "[] (red ->(! X green)) ;\n"^"[] (red -> (<> green));\n"^"(red -> (! (green U yellow))); \n" ^"[] (red -> ((<> green) && (! (green U yellow)))); \n" ^"(red-> X (red U (yellow && X (yellow U green))));\n" ;;


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
  ;;

let rec translateLTL (ltl:ltl) :es =
  match ltl with 
    Lable str -> Event (str, None)
  | Next l -> Cons (Underline, translateLTL l) 
  | Until (l1, l2) -> Cons (Kleene (translateLTL l1), translateLTL l2)
  | Global l -> Omega (translateLTL l)
  | Future l -> Cons (Kleene (Underline), translateLTL l)
  | NotLTL l -> Not (translateLTL l)
  | Imply (l1, l2) -> ESOr (Not (translateLTL l1), ESAnd (translateLTL l1, translateLTL l2))
  | AndLTL (l1, l2) -> ESAnd (translateLTL l1, translateLTL l2)
  ;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"


let main = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let specs:(string list) =  (input_lines ic) in 
    let lines = List.fold_right (fun x acc -> acc ^ "\n" ^ x) ( specs) "" in 
    let ltlList:(ltl list) = Parser.ltl_p Lexer.token (Lexing.from_string  lines)  in

    let esList=List.map (fun ltl-> translateLTL ltl) (ltlList)  in 
    let producte = List.combine ltlList esList in
    let result = List.fold_right (fun (l,e) acc -> acc ^ showLTL l ^ " ==> "^(showES e) ^"\n\n") (producte)  "" in 
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s\n" result;   (* 写一些东西 *)

    (*
    print_string (showLTLList ^ "\n==============\n");
    *)
    print_string (result^"\n");

    close_out oc;                (* 写入并关闭通道 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;

  