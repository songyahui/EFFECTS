open String
open List
open Ast
open Printf
open Parser
open Lexer
open Pretty
open Rewriting 


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
  let rec printarg v = (match v with
  Unit  -> "unit"
  | Integer num -> string_of_int num
  | Bool b -> string_of_bool b 
  | Float f -> string_of_float f
  | Variable v -> v 
  | Call (name, elist) -> name ^ "(" ^ print_real_Param elist ^ ")"
  | BinOp (e1, e2, str) -> printarg e1 ^ str ^ printarg e2 
  | _ -> "undefined"
  ) in 
  match params with 
    [] -> ""
  | [v] ->  printarg v
    
  | v::xs ->  
    let pre = printarg v in 
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
  | Cond (e1, e2, str) -> printExpr e1 ^ str ^ printExpr e2 
  | BinOp (e1, e2, str) -> printExpr e1 ^ str ^ printExpr e2 
  
  
  ;;


let rec printSpec (s:spec ) :string = 
  match s with 
  PrePost (e1, e2) -> "\n[Pre: " ^ showEffect e1 ^ "]\n[Post:"^ showEffect e2 ^"]\n"



let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"


let rec concatEffEs (eff:effect) (es:es) : effect = 
  match eff with 
    Effect (p,e) -> Effect (p, Cons (e, es))
  | Disj (eff1, eff2) -> Disj ((concatEffEs eff1 es), (concatEffEs eff2 es));; 
 

let rec concatEffEff (eff1:effect) (eff2:effect) : effect = 
  match eff1 with 
    Effect (p1,e1) -> 
      (match eff2 with
        Effect (p2,e2) -> Effect (PureAnd(p1,p2) , Cons(e1, e2))
      | Disj (ef1, ef2) -> Disj ((concatEffEff eff1 ef1), (concatEffEff eff1 ef2))
      )
  | Disj (ef1, ef2) -> 
      (match eff2 with
        Effect (p2,e2) -> Disj ((concatEffEff ef1 eff2), (concatEffEff ef1 eff2))
      | Disj (_, _ ) -> Disj ((concatEffEff ef1 eff2), (concatEffEff ef2 eff2))
      )
      ;;

let rec searMeth (prog: program) (name:string) : meth option= 
  match prog with 
    [] -> None
  | x::xs -> 
    (match x with 
      Include str -> searMeth xs name
    | Method (Meth (t, mn , list_parm, PrePost (pre, post), expression)) -> 
      if mn = name then Some (Meth (t, mn , list_parm, PrePost (pre, post), expression))
      else searMeth xs name 
    )
    ;;


let checkPrecondition (state:effect) (pre:effect) : bool = 
  let reverseState = reverseEff state in
  let reversePre = reverseEff pre in 
  (*check containment*)
  let (result_tree, result) =  Rewriting.containment reverseState  reversePre [] in 
  result;;

let rec verifier (expr:expression) (state:effect) (prog: program): effect = 
  match expr with 
    Unit -> state
  | EventRaise ev -> concatEffEs state (Event ev)
  | Seq (e1, e2) -> 
    let state' = verifier e1 state prog in 
    verifier e2 state' prog
  | IfElse (e1, e2, e3) -> Disj ((verifier e2 state prog), (verifier e3 state prog))
  | Call (name, exprList) -> 
    (match searMeth prog name with 
      None -> raise (Foo ("Method: "^ name ^" not defined!"))
    | Some me -> 
      (match me with 
        Meth (t, mn , list_parm, PrePost (pre, post), expression) -> 
          if checkPrecondition state pre == true then 
          concatEffEff state post
          else raise (Foo ("PreCondition does not hold: "^ name ^"!"))
      )
    )
  | _ -> state
    ;;

let rec verification (dec:declare) (prog: program): string = 
  match dec with 
    Include str -> ""
  | Method (Meth (t, mn , list_parm, PrePost (pre, post), expression)) -> 
    showEffect (verifier expression (pre) prog) ^ "\n"
 ;;

let rec printMeth (me:meth) :string = 
  match me with 
    Meth (t, mn , list_parm, PrePost (pre, post), expression) -> 
    let p = printType t ^ mn^ "(" ^ printParam list_parm ^ ") "^ printSpec (PrePost (pre, post))^"{"^ printExpr expression ^"}\n" in
    p 
    ;;

let rec printProg (pram: declare list) :string =
  match pram with 
    [] -> ""
  | x::xs -> 
    let str = (match x with 
              Include str -> str ^ "\n" 
            | Method me -> printMeth me 
    )in
    str ^ printProg xs ;;

let () = 
    let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in 
    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
    let ic = open_in inputfile in
    try 
      let lines =  (input_lines ic ) in  
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in 
      let prog = Parser.prog Lexer.token (Lexing.from_string line) in
      let testprintProg = printProg prog in 
      let verification_re = List.fold_right (fun dec acc -> acc ^ (verification dec prog)) prog ""  in
      let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
      fprintf oc "%s\n" verification_re;   (* 写一些东西 *)
      close_out oc;                (* 写入并关闭通道 *)
      print_string testprintProg;
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *) 
  
    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  
   ;;

