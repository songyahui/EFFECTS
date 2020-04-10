open Random
open Ast
open Pretty
open Printf
open List
open String
open Rewriting

let test = "[]<>green;\n"^ "[] (red ->(! X green)) ;\n"^"[] (red -> (<> green));\n"^"(red -> (! (green U yellow))); \n" ^"[] (red -> ((<> green) && (! (green U yellow)))); \n" ^"(red-> X (red U (yellow && X (yellow U green))));\n" ;;




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

let rec translateLTL (pi:pure) (ltl:ltl) (varList:string list) :(pure * es * string list) =
  match ltl with 
    Lable str -> (pi, Event (str, None), varList)
  | Next l -> 
    let (piii, ess, varList') =  translateLTL pi l varList in 
    (piii, Cons (Underline, ess), varList')
  | Until (l1, l2) -> 
      let newVar = getAfreeVar varList in 
      let newPi = PureAnd (pi, Gt (Var newVar, Number 0)) in 
      let (pi1, ess1, varList1) =  translateLTL newPi l1 (newVar :: varList) in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      let prefix = Ttimes (ess1, Var newVar) in 
      (pi2, Cons (prefix, ess2), varList2)
  | Global l -> 
      let (piii , ess1, varList') =  translateLTL pi l varList in 

      (piii, Kleene (ess1), varList')
  | Future l -> 
      let newVar = getAfreeVar varList in 
      let prefix = Ttimes (Underline, Var newVar) in 
      let (piii, ess, varList') =  translateLTL pi l (newVar::varList) in 
 
      (piii, Cons (prefix, ess), varList')
  | NotLTL l -> 
      let (piii, ess, varList') =  translateLTL pi l varList in 
      (piii, Not (ess), varList')
  | Imply (l1, l2) -> 
      let (pi1, ess1, varList1) =  translateLTL pi l1 varList in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      (pi2, ESOr ( (Not (ess1)),  Cons (ess1, ess2)), varList2)
  | AndLTL (l1, l2) -> 
      let (pi1, ess1, varList1) =  translateLTL pi l1 varList in 
      let (pi2, ess2, varList2) =  translateLTL pi1 l2 varList1 in 
      (pi2, ESAnd (ess1, ess2), varList2)
  ;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

  ;;

let get_0 (a,_) = a ;;

let get_1 (_, a,_) = a ;;

let trafic = "(( Green . Yellow . Red )^w)";;


let testASingle pi ff ss :string = 
  print_string (ff ^"  "^ss^"\n");
  let lhs = addConstrain (Effect(TRUE, (Parser.es_p Lexer.token (Lexing.from_string ff)))) pi  in
  let rhs = addConstrain (Effect(TRUE, (Parser.es_p Lexer.token (Lexing.from_string ss)))) pi  in
  (*let rhs:(es) = Parser.es_p Lexer.token (Lexing.from_string  ss)  in*)
  printReport lhs rhs
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

    let result = List.fold_right (fun (l,e) acc -> acc ^ showLTL l ^ " ==> "^(showEffect e) ^"\n\n") (producte)  "" in 
    print_string (result^"\n");
    
    
    
    
    fprintf oc "%s\n" result;   (* 写一些东西 *)


    close_out oc;                (* 写入并关闭通道 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *) 

  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

 ;;

  