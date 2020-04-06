open Random
open Ast
open Pretty
open Printf
open List
open String

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

let rec translateLTL (ltl:ltl) (varList:string list) :(es * string list) =
  match ltl with 
    Lable str -> (Event (str, None), varList)
  | Next l -> 
    let (ess, varList') =  translateLTL l varList in 
    (Cons (Underline, ess), varList')
  | Until (l1, l2) -> 
      let newVar = getAfreeVar varList in 
      let (ess1, varList1) =  translateLTL l1 (newVar :: varList) in 
      let (ess2, varList2) =  translateLTL l2 varList1 in 
      let prefix = Ttimes (ess1, Var newVar) in 
      (Cons (prefix, ess2), varList2)
  | Global l -> 
      let (ess, varList') =  translateLTL l varList in 
      (Omega (ess), varList')
  | Future l -> 
      let newVar = getAfreeVar varList in 
      let prefix = Ttimes (Underline, Var newVar) in 
      let (ess, varList') =  translateLTL l (newVar::varList) in 
      (Cons (prefix, ess), varList')
  | NotLTL l -> 
      let (ess, varList') =  translateLTL l varList in 
      (Not (ess), varList')
  | Imply (l1, l2) -> 
      let (ess1, varList1) =  translateLTL l1 varList in 
      let (ess2, varList2) =  translateLTL l2 varList1 in 
      (ESOr (Not (ess1), ESAnd (ess1, ess2)), varList2)
  | AndLTL (l1, l2) -> 
      let (ess1, varList1) =  translateLTL l1 varList in 
      let (ess2, varList2) =  translateLTL l2 varList1 in 
      (ESAnd (ess1, ess2), varList2)
  ;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"


let get_0 (a,_) = a ;;

let main = 
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
  let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
  let ic = open_in inputfile in
  try 
    let specs:(string list) =  (input_lines ic) in 
    let lines = List.fold_right (fun x acc -> acc ^ "\n" ^ x) ( specs) "" in 
    let ltlList:(ltl list) = Parser.ltl_p Lexer.token (Lexing.from_string  lines)  in

    let esList=List.map (fun ltl-> get_0 (translateLTL ltl []) ) (ltlList)  in 
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

  