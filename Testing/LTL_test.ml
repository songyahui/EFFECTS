open Random
open Ast
open Pretty
open Printf
open List
open Antimirov


let rec translateLTL (ltl:ltl) :(es) =
  match ltl with 
    Lable str -> Event (str, None)
  | Next l -> 
    Cons (Underline, translateLTL l)
  | Until (l1, l2) -> 
    Cons (Kleene (translateLTL l1), translateLTL l2)
  | Global l -> 
      Kleene (translateLTL l)
  | Future l -> 
      Cons (Kleene (Underline), translateLTL l)  
  | NotLTL l -> 
      Not (translateLTL l)
  | Imply (l1, l2) -> 
      let ess1 =  translateLTL l1 in 
      let ess2 =  translateLTL l2 in 
      ESOr (Not (ess1), ESAnd (ess1, ess2))
  | AndLTL (l1, l2) -> 
      let ess1 =  translateLTL l1 in 
      let ess2 =  translateLTL l2 in 
      ESAnd (ess1, ess2)
  ;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"
;;

let trafic = "(( Green . Yellow . Red )^w)";;
let spe = "(((_^*).Green)^w)" ;;


let testASingle ff ss :string = 
  let lhs:(es) = Parser.es_p Lexer.token (Lexing.from_string  ff)  in
  (*let rhs:(es) = Parser.es_p Lexer.token (Lexing.from_string  ss)  in*)
  let (a, b, c) = antimirov_shell lhs ss in
  string_of_bool a ^ "\n" ^ string_of_int b ^"\n" ^string_of_float c^"\n";;


let main = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let specs:(string list) =  (input_lines ic) in 
    let lines = List.fold_right (fun x acc -> acc ^ "\n" ^ x) ( specs) "" in 
    let ltlList:(ltl list) = Parser.ltl_p Lexer.token (Lexing.from_string  lines)  in

    let esList=List.map (fun ltl-> (translateLTL ltl ) ) (ltlList)  in 


    let producte = List.combine ltlList esList in
    let result = List.fold_right (fun (l,e) acc -> acc ^ showLTL l ^ " ==> "^(showES e) ^"\n\n") (producte)  "" in 
    let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s\n" result;   (* 写一些东西 *)

    (*
    print_string (showLTLList ^ "\n==============\n");
    *)
    print_string (result^"\n");

    
        let ential_result = List.fold_right (fun x acc -> acc ^ testASingle trafic x) esList "" in 

    print_string (ential_result^"\n");

    close_out oc;                (* 写入并关闭通道 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
    ;;

  (*
  
  *)