open String
open List
open Ast
open Printf
open Parser
open Lexer
open Pretty


let rec printType (ty:_type) :string =
  match ty with
    INT -> "int "
  | FLOAT -> "float "
  | BOOL  -> "bool "
  | VOID  -> "void ";;

let rec printParam (params: param):string = 
  match params with 
    [] -> ""
  | [(t, v)] -> printType t ^ v
  | (t, v)::xs ->  printType t ^ v ^ "," ^ printParam xs ;;

let rec print_real_Param (params: expression list):string = 
  match params with 
    [] -> ""
  | [v] ->  
    (match v with
    Unit  -> "unit"
    | Integer num -> string_of_int num
    | Bool b -> string_of_bool b 
    | Float f -> string_of_float f
    | Variable v -> v 
    | Call (name, elist) -> name ^ "(" ^ print_real_Param elist ^ ")"
    | _ -> "undefined"
    )
  | v::xs ->  
    let pre = (match v with
    Unit  -> "unit"
    | Integer num -> string_of_int num
    | Bool b -> string_of_bool b 
    | Float f -> string_of_float f
    | Variable v -> v 
    | Call (name, elist) -> name ^ "(" ^ print_real_Param elist ^ ")"
    | _ -> "undefined"
    ) in 
    pre ^ "," ^ print_real_Param xs ;;

let rec printExpr (expr: expression):string = 
  match expr with 
    Unit  -> "unit"
  | Integer num -> string_of_int num
  | Bool b -> string_of_bool b 
  | Float f -> string_of_float f
  | Variable v -> v 
  | LocalDel (t, v, e)->  printType t ^ v ^ " = " ^ printExpr e
  | Call (name, elist) -> name ^ "(" ^ print_real_Param elist ^ ")"
  | Assign (v, e) -> v ^ " = " ^ printExpr e
  | Seq (e1, e2) -> printExpr e1 ^ "\n" ^ printExpr e2
  | EventRaise ev -> ev
  | IfElse (e1, e2, e3) -> "if " ^ printExpr e1 ^ " then " ^ printExpr e2 ^ " else " ^ printExpr e3 
  ;;

let rec printSpec (s:spec ) :string = 
  match s with 
  PrePost (e1, e2) -> "\n[Pre: " ^ showEffect e1 ^ "]\n[Post:"^ showEffect e2 ^"]\n"


let rec printMeth (me:meth) :string = 
  match me with 
    Meth (t, mn , list_parm, spec, expression) -> 
    printType t ^ mn^ "(" ^ printParam list_parm ^ ") "^ printSpec spec^"{"^ printExpr expression ^"}\n";;


let rec printProg (prog: meth list) :string =
  match prog with 
    [] -> ""
  | x::xs -> printMeth x ^ printProg xs ;;


let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"
 

let () = 
    let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
    let ic = open_in inputfile in
    try 
      let lines =  (input_lines ic ) in  (* 从输入通道读入一行并丢弃'\n'字符 *)
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in 
      
      let me = Parser.meth Lexer.token (Lexing.from_string line) in
      let result = printMeth me in 
      print_string result;
      close_in ic                  (* 关闭输入通道 *) 
  
    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  
   ;;

