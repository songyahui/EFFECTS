open String
open List
open Ast
open Printf
open Parser
open Lexer
open Pretty
open Rewriting 
open Sys


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
  | Return  -> "return"
  | Integer num -> string_of_int num
  | Bool b -> string_of_bool b 
  | Float f -> string_of_float f
  | String s -> "\"" ^ s^"\""
  | Variable v -> v 
  | LocalDel (t, v, e)->  printType t ^ v ^ " = " ^ printExpr e
  | Call (name, elist) -> name ^ "(" ^ print_real_Param elist ^ ")"
  | Assign (v, e) -> v ^ " = " ^ printExpr e
  | Seq (e1, e2) -> printExpr e1 ^ ";" ^ printExpr e2
  | EventRaise (ev,None) -> ev
  | EventRaise (ev,Some n) -> ev ^"(" ^string_of_int n ^")"
  | Deadline (e, n) -> "deadline (" ^ printExpr e ^", " ^ string_of_int n ^")\n"
  | Timeout (e, n) -> "timeout (" ^ printExpr e ^", " ^ string_of_int n ^")\n"

  | Delay n -> "delay " ^  string_of_int n ^"\n"
  | IfElse (e1, e2, e3) -> "if " ^ printExpr e1 ^ " then " ^ printExpr e2 ^ " else " ^ printExpr e3 
  | Cond (e1, e2, str) -> printExpr e1 ^ str ^ printExpr e2 
  | BinOp (e1, e2, str) -> printExpr e1 ^ str ^ printExpr e2 
  | Assertion eff -> "Assert: " ^ showEffect eff 
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
  | Disj (eff1, eff2) -> Disj ((concatEffEs eff1 es), (concatEffEs eff2 es))
  ;; 
 

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
  | GtEq (term, n) ->  GtEq (substituteTermWithAgr term realArg formalArg, n)
  | LtEq (term, n) ->  LtEq (substituteTermWithAgr term realArg formalArg, n)
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
  let realArgs' = List.filter (fun x -> 
                                match x with 
                                Unit -> false 
                              | _-> true ) realArgs in 

  let formalArgs = List.map (fun (a, b) -> b) formal in 
  let pairs = List.combine realArgs' formalArgs in 
  let rec subArgOnebyOne (eff:effect) (pairs:(expression * var ) list): effect = 
    (match pairs with 
      [] -> eff 
    | (realArg, formalArg):: xs  -> 
      let subThisPair = substituteEffWithAgr eff realArg formalArg in
      subArgOnebyOne subThisPair xs 
    )
  in subArgOnebyOne eff pairs;;



(*n >0 /\ A^n  -> n = 2 /\ n > 0 /\ A^n
let substituteEffWithPure (eff:effect) (realArgs: expression list) (formal: (_type * var) list ) =
    let exprToTerm (ex:expression):terms = 
      match ex with 
        Integer num -> Number num
      | _ -> print_string (printExpr ex^"\n");
      raise (Foo "substituteEffWithPure");
    in 
    let realArgs' = List.filter (fun x -> 
                                match x with 
                                Unit -> false 
                              | _-> true ) realArgs in 
    let formalArgs = List.map (fun (a, b) -> b) formal in 
    let pairs = List.combine realArgs' formalArgs in 
    let constrains = List.map (fun (a, b) -> Eq (Var b, exprToTerm a )) pairs in 

    let consNew = List.fold_right (fun con acc -> PureAnd (acc, con ) ) (constrains) TRUE in 
    addConstrain eff consNew
    ;;

*)

let checkPrecondition (state:effect) (pre:effect)  = 
  let reverseState =  (reverseEff state) in
  let reversePre =  (reverseEff pre) in 
  (*check containment*)
  let (result_tree, result, states, hypo) =  Rewriting.printReportHelper reverseState reversePre false in 
  let tree = Node (showEntailmentEff reverseState reversePre, [result_tree]) in

  if result == false then 
  let printTree = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  print_string printTree;
  (result, tree)

  else 
  
  (result, tree);;

let condToPure (expr :expression) :pure = 
  match expr with 
    Cond (Variable v, Integer n, "==")  -> Eq (Var v, Number n)
  | Cond (Variable v, Integer n, "<=")  -> PureOr(Eq (Var v, Number n),Lt (Var v, Number n))
  | Cond (Variable v, Integer n, ">=")  -> PureOr(Eq (Var v, Number n),Gt (Var v, Number n))
  | Cond (Variable v, Integer n, ">")  -> Gt (Var v, Number n)
  | Cond (Variable v, Integer n, "<")  -> Lt (Var v, Number n)
  | _ -> raise (Foo "exception in condToPure")
  ;;

let rec verifier (caller:string) (expr:expression) (state_H:effect) (state_C:effect) (prog: program): effect = 
  match expr with 
    EventRaise (ev,p) -> concatEffEs state_C (Event (ev,p))
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
  | Assertion eff ->   
    let his_cur =  (concatEffEff state_H state_C) in 
    let (result, tree) = checkPrecondition (his_cur) eff in 
    if result == true then state_C 
    else raise (Foo ("Assertion " ^ showEffect eff ^" does not hold!"))
            
  | Call (name, exprList) -> 
    (match searMeth prog name with 
      None -> 
       if (String.compare name "printf" == 0) then state_C
       else raise (Foo ("Method: "^ name ^" not defined!"))
    | Some me -> 
      (

        match me with 
          Meth (t, mn , list_parm, PrePost (pre, post), expression) -> 
          
            
            let subPre = substituteEffWithAgrs pre exprList list_parm in 
            let subPost = substituteEffWithAgrs post exprList list_parm in 
            (*
            let subPre = substituteEffWithPure pre exprList list_parm in 
            let subPost = substituteEffWithPure post exprList list_parm in 
*)
            let his_cur =  (concatEffEff state_H state_C) in 

            let (result, tree) = checkPrecondition (his_cur) subPre in 
            (*print_string ((printTree ~line_prefix:"* " ~get_name ~get_children tree));*)
            
            if result == true then 
              (
                (*print_string ("[Precondition holds] when " ^caller ^" is calling " ^ mn ^"\n\n");*)
              let newState = ( (concatEffEff ( state_C) ( subPost))) in
              newState)
            else 
            
            raise (Foo ("PreCondition does not hold when " ^ caller^" is calling: "^ name ^"!"))
            
      
      )
    )
  | _ -> state_C
    ;;

let rec extracPureFromPrecondition (eff:effect) :effect = 
  match eff with 
    Effect (pi, es) -> Effect (pi, Emp)
  | Disj (eff1, eff2) -> Disj (extracPureFromPrecondition eff1, extracPureFromPrecondition eff2)
  ;;

let rec verification (decl:(bool * declare)) (prog: program): string = 
  let (isIn, dec) = decl in 
  if isIn == false then ""
  else 
  let startTimeStamp = Sys.time() in
  match dec with 
    Include str -> ""
  | Method (Meth (t, mn , list_parm, PrePost (pre, post), expression)) -> 
    let head = "[Verification for method: "^mn^"]\n"in 
    let precon = "[Precondition: "^(showEffect ( pre)) ^ "]\n" in
    let postcon = "[Postcondition: "^ (showEffect ( post)) ^ "]\n" in 
    let acc =  (verifier mn expression (pre) (extracPureFromPrecondition pre) prog) in 
    
    let accumulated = "[Real Effect: " ^(showEffect ( normalEffect acc 0)) ^ "]\n" in 
    (*print_string((showEntailmentEff acc post) ^ "\n") ;*)
    
    (*let varList = (*append*) (getAllVarFromEff acc) (*(getAllVarFromEff post)*) in  
    *)
    let (result_tree, result, states, hypos) =  Rewriting.printReportHelper acc post false in 
    let result = "[Result: "^ (if result then "Succeed" else "Fail") ^"]\n" in 
    let states = "[Explored "^ string_of_int (states+1)  ^ " States]\n" in 
    let verification_time = "[Verification Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in
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

let getIncl (d:declare) :bool = 
  match d with 
    Include str -> (String.compare str "primitives.c") != 0
  | _ -> false 
  ;;



let rec getIncludedFiles (p:(bool * declare) list) :(bool * declare) list = 
  let readFromFile (name:string):(bool * declare) list  = 
    let currentP = split_on_char '/' (Sys.getcwd ()) in 
    let serverOrNot = List.exists (fun a -> String.compare a "cgi-bin" == 0) currentP in 

    let inputfile = if serverOrNot then (Sys.getcwd () ^ "/../src/program/" ^ name) 
                    else (Sys.getcwd () ^ "/src/program/" ^ name) 
    in
    let ic = open_in inputfile in
    try 
      let lines =  (input_lines ic ) in  
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in 
      let raw_prog = List.map (fun a -> (false, a)) (Parser.prog Lexer.token (Lexing.from_string line)) in
      let prog = getIncludedFiles raw_prog in 
  
      close_in ic;                  (* 关闭输入通道 *) 
      prog
    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  in 
  let incl = List.filter (fun (ind, x) -> getIncl x) (p) in 
  let getName:(string list ) = List.map (fun (ind, x) -> 
                              match x with 
                              Include str -> str
                            | _ -> "") incl in
  let appendUp  = List.fold_right (fun (x) acc -> append (readFromFile x) acc ) (getName) p in 
 
  appendUp;;


let () =
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in
(*    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
print_string (inputfile ^ "\n" ^ outputfile^"\n");*)
  let ic = open_in inputfile in
  try
      let lines =  (input_lines ic ) in
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
      let raw_prog = List.map (fun a -> (true, a)) (Parser.prog Lexer.token (Lexing.from_string line)) in
      let prog = getIncludedFiles raw_prog in

      (*
      let testprintProg = printProg prog in 
      print_string testprintProg;
*)
      let evn = List.map (fun (ind, a) -> a) prog in
      let verification_re = List.fold_right (fun dec acc -> acc ^ (verification dec evn)) prog ""  in
      (*let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
      (*      let startTimeStamp = Sys.time() in*)
      (*fprintf oc "%s\n" verification_re;   (* 写一些东西 *)*)
      print_string (verification_re ^"\n");
      (*print_string (string_of_float(Sys.time() -. startTimeStamp)^"\n" );*)
      close_out oc;                (* 写入并关闭通道 *)
      *)
      print_string (verification_re ^"\n");
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;
