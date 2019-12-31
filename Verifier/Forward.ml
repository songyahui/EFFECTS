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
  | Seq (e1, e2) -> printExpr e1 ^ ";" ^ printExpr e2
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
        Effect (p2,e2) -> Disj ((concatEffEff ef1 eff2), (concatEffEff ef2 eff2))
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




let rec substitutePureWithAgr (pi:pure) (realArg:expression) (formalArg: var):pure = 
  match pi with 
    TRUE -> pi 
  | FALSE ->pi
  | Gt (term, n) ->  Gt (substituteTermWithAgr term realArg formalArg, n)
  | Lt (term, n) ->  Lt (substituteTermWithAgr term realArg formalArg, n)
  | Eq (term, n) ->  Eq (substituteTermWithAgr term realArg formalArg, n)
  | PureOr (p1, p2) -> PureOr (substitutePureWithAgr p1 realArg formalArg, substitutePureWithAgr p2 realArg formalArg)
  | PureAnd (p1, p2) -> PureAnd (substitutePureWithAgr p1 realArg formalArg, substitutePureWithAgr p2 realArg formalArg)
  | Neg p -> Neg (substitutePureWithAgr p realArg formalArg)
  ;;




let rec substituteEffWithAgr (eff:effect) (realArg:expression) (formalArg: var):effect = 
  match eff with 
    Effect (pi, es) -> Effect (substitutePureWithAgr pi realArg formalArg, substituteESWithAgr es realArg formalArg)
  | Disj (eff1, eff2) -> Disj (substituteEffWithAgr eff1 realArg formalArg, substituteEffWithAgr eff2 realArg formalArg)
  ;;

let substituteEffWithAgrs (eff:effect) (realArgs: expression list) (formal: (_type * var) list ) =
  let formalArgs = List.map (fun (a, b) -> b) formal in 
  let pairs = List.combine realArgs formalArgs in 
  let rec subArgOnebyOne (eff:effect) (pairs:(expression * var ) list): effect = 
    (match pairs with 
      [] -> eff 
    | (realArg, formalArg):: xs  -> 
      let subThisPair = substituteEffWithAgr eff realArg formalArg in
      subArgOnebyOne subThisPair xs 
    )
  in subArgOnebyOne eff pairs;;


let checkPrecondition (state:effect) (pre:effect)  = 
  let reverseState =  (reverseEff state) in
  let reversePre =  (reverseEff pre) in 
  (*check containment*)
  let varList = (*append*) (getAllVarFromEff reverseState) (*(getAllVarFromEff reversePre) *)in  
  let (result_tree, result, states) =  Rewriting.containment reverseState reversePre [] varList in 
  (*if result == false then 
  let printTree = printTree ~line_prefix:"* " ~get_name ~get_children result_tree in
  print_string printTree;
  raise(Foo ("=============================\n") );
  else 
  *)
  (result, result_tree);;

let condToPure (expr :expression) :pure = 
  match expr with 
    Cond (Variable v, Integer n, "==")  -> Eq (Var v, n)
  | Cond (Variable v, Integer n, "<=")  -> PureOr(Eq (Var v, n),Lt (Var v, n))
  | Cond (Variable v, Integer n, ">=")  -> PureOr(Eq (Var v, n),Gt (Var v, n))
  | Cond (Variable v, Integer n, ">")  -> Gt (Var v, n)
  | Cond (Variable v, Integer n, "<")  -> Lt (Var v, n)
  | _ -> raise (Foo "exception in condToPure")
  ;;

let rec verifier (caller:string) (expr:expression) (state_H:effect) (state_C:effect) (prog: program): effect = 
  match expr with 
    EventRaise ev -> concatEffEs state_C (Event ev)
  | Seq (e1, e2) -> 
    let state_C' = verifier caller e1 state_H state_C prog in 
    verifier caller e2 state_H state_C' prog
  | Assign (v, e) -> verifier caller e state_H state_C prog 
  | LocalDel (t, v , e) ->   verifier caller e state_H state_C prog      
  | IfElse (e1, e2, e3) -> 
    let condIf = condToPure e1 in 
    let condElse = Neg condIf in 
    let state_C_IF  = addConstrain state_C condIf in 
    let state_C_ELSE  = addConstrain state_C condElse in 
    Disj ((verifier caller e2 state_H state_C_IF prog), (verifier caller e3 state_H state_C_ELSE prog))
  | Call (name, exprList) -> 
    (match searMeth prog name with 
      None -> raise (Foo ("Method: "^ name ^" not defined!"))
    | Some me -> 
      (

        match me with 
          Meth (t, mn , list_parm, PrePost (pre, post), expression) -> 
            let subPre = substituteEffWithAgrs pre exprList list_parm in 
            let subPost = substituteEffWithAgrs post exprList list_parm in 
            let his_cur =  (concatEffEff state_H state_C) in 
            let (result, tree) = checkPrecondition (his_cur) subPre in 
            (*print_string ((printTree ~line_prefix:"* " ~get_name ~get_children tree));*)
            
            if result == true then 
              (
                (*print_string ("[Precondition holds] when " ^caller ^" is calling " ^ mn ^"\n\n");*)
              let newState = ( (concatEffEff ( state_C) ( subPost))) in
              newState)
            else 
            raise (Foo ("PreCondition does not hold when " ^ caller^" is calling: "^ name ^"!\n"))
      
      )
    )
  | _ -> state_C
    ;;

let rec verification (dec:declare) (prog: program): string = 
  let startTimeStamp = Sys.time() in
  match dec with 
    Include str -> ""
  | Method (Meth (t, mn , list_parm, PrePost (pre, post), expression)) -> 
    let head = "[Verification for method: "^mn^"]\n"in 
    let precon = "[Precondition: "^(showEffect ( pre)) ^ "]\n" in
    let postcon = "[Postcondition: "^ (showEffect ( post)) ^ "]\n" in 
    let acc =  (verifier mn expression (pre) (Effect (TRUE, Emp)) prog) in 
    let accumulated = "[Real Effect: " ^(showEffect ( acc )) ^ "]\n" in 
    (*print_string((showEntailmentEff acc post) ^ "\n") ;*)
    
    let varList = (*append*) (getAllVarFromEff acc) (*(getAllVarFromEff post)*) in  
    let (result_tree, result, states) =  Rewriting.containment acc ( post) [] varList in 
    let result = "[Result: "^ string_of_bool result ^"]\n" in 
    let states = "[Explored "^ string_of_int (states+1)  ^ " States]\n" in 
    let verification_time = "[Verification Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ "]\n" in
    let printTree = printTree ~line_prefix:"* " ~get_name ~get_children result_tree in
    "=======================\n"^ head ^ precon ^ accumulated ^ postcon ^ result ^ states ^verification_time^ "\n" ^ printTree ^ "\n" 
    
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
      (*let testprintProg = printProg prog in 
      print_string testprintProg;*)
      let verification_re = List.fold_right (fun dec acc -> acc ^ (verification dec prog)) prog ""  in
      let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
      let startTimeStamp = Sys.time() in
      fprintf oc "%s\n" verification_re;   (* 写一些东西 *)
      (*print_string (string_of_float(Sys.time() -. startTimeStamp)^"\n" );*)
      close_out oc;                (* 写入并关闭通道 *)
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *) 
  
    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  
   ;;


(*
1. why it takes so long
2. gcc 
3. comparaasion with antichain
*)