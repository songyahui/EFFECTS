open Random
open Ast
open Pretty
open Printf
open List
open Antimirov
open RegToNfa

type op = OpCon | OpStar | OpUinon

let showOp (o:op) :string = 
  match o with 
    OpCon  -> "OpCon\n"
  | OpStar -> "OpStar\n"
  | OpUinon-> "OpUinon\n"
  ;;

let alphabet = ["A"; "B"]

let height = 5;;
let sigma = 2;;
let sampleNum = 100;;

let getRandomeOp (num:int):op = 
  match num with 
    0 -> OpCon
  | 1 -> OpStar
  | 2 -> OpUinon 
  | _ -> raise (Foo "randomeOp exception")
  ;;

let getRandomEvent (s:int): string =
  Random.self_init ();
  let ev = Random.int s in
  List.nth alphabet ev
  ;;


let rec regexGen (h:int) (s:int) : es = 
  if h <= 0 then Event (getRandomEvent s)
  else 
    (Random.self_init ();
    let num = Random.int 3 in 
    let op = getRandomeOp num in 
    match op with 
      OpCon -> 
        let es1 = regexGen (h-1) s in 
        let es2 = regexGen (h-1) s in
        Cons (es1, es2)
    | OpStar -> Kleene (regexGen (h-1) s)
    | OpUinon -> 
        let es1 = regexGen (h-1) s in 
        let es2 = regexGen (h-1) s in
        ESOr (es1, es2)
    )
;;

let cartesian l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)
;;

let getFst (a, b, c) = a ;;

let rec genES (num:int) (acc:es list): es list= 
  if num = 0 then acc
  else 
  (let one =  regexGen height sigma in 
  genES (num - 1) (append acc [one] );
  ) (**)
;;

let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail
;;

let main =
  (*let ess = genES sampleNum  [] in
  let states = List.map (fun a -> RegToNfa.getStates (showESReg a)) ess in 
  print_string ("[");
  List.fold_left (fun acc a ->  print_string(string_of_int a ^",")) () states ;
  print_string ("]");

  let line = "\n" in
  let EE (Effect (p1,lhs), Effect (p2,rhs)) = Parser.ee Lexer.token (Lexing.from_string line) in
  (*let result = printReport (Effect (p1,lhs)) (Effect (p2,rhs)) in *)
  let (a,b) = Antimirov.antimirov lhs rhs [] in 
  print_string(string_of_bool a ^":"^string_of_int b);;
  *)

(************OUTPUT TO FILE************)
  
  let ess = genES sampleNum  [] in
  let pairs = cartesian ess ess (*[(Cons(Event "B", Cons(Event "B", Kleene(Event "B"))),Cons (Kleene(Event "B"), Cons(Event "B",Event "B") ))] *)in 
  
  let outputfile = (Sys.getcwd ()^ "/" ^ "Testing/regex"^ string_of_int height ^".dat") in

  let dataset' = List.fold_left (fun acc (lhs, rhs) -> acc ^ showEntailmentESReg lhs rhs ^"\n") "" pairs in 
  
  let dataset = List.fold_left (fun acc (lhs, rhs) -> acc ^ showEntailmentES lhs rhs ^"\n") "" pairs in 

  let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s" (dataset'^"\n"^dataset);   (* 写一些东西 *)
    close_out oc;


  let rowData:(Ast.es * int * Ast.es * int) list = List.map (fun (lhs, rhs) -> (lhs, RegToNfa.getStates (showESReg lhs) ,rhs, RegToNfa.getStates (showESReg rhs) ) ) pairs in 

(************Get the resulrs************)
(*
let temp1 = "" in 
let temp2 = "" in 



let temppairs = [(Parser.es_p Lexer.token (Lexing.from_string temp1)
, Parser.es_p Lexer.token (Lexing.from_string temp2))] in 
*)


  let resultsChain = List.map (fun (lhs, rhs) -> RegToNfa.antichain (showESReg lhs) (showESReg rhs)) pairs in 
  let resultsMirov = List.map (fun (lhs, rhs) -> (Antimirov.antimirov_shell lhs rhs )) pairs in 

(************Output resulrs to files************)

  let rec pairList lst1 lst2 lst3 = 
    match (lst1, lst2,lst3 ) with
      ([], [], []) -> [] 
    | (x::xs, y ::ys , z::zs) -> (x, y, z) :: pairList xs ys zs
    | _ -> raise (Foo "pairList")
  in 
  let pairResults = pairList resultsChain resultsMirov rowData in 
  


  let head = "" in 
  let format_abc ((a, b , c):(bool * int * float)) :string = (if a then "1" else "0") ^ ", " ^ string_of_int b ^ ", " ^ string_of_float c ^ " " in 
  let format_row ((lhs, lshS, rhs, rhsS): (es* int* es* int)) :string = showEntailmentES lhs rhs ^":" ^showEntailmentESReg lhs rhs  ^", " ^  string_of_int lshS ^ ", " ^ string_of_int rhsS ^ " " in 

  let finalPrint = List.fold_left (fun acc (chain, mirov, row) -> acc^ format_row row^ ", " ^ format_abc chain ^", "^ format_abc mirov ^ "\n") head pairResults in
  let outputResultfile = (Sys.getcwd ()^ "/" ^ "DataAnylase/data/result_height_"^ string_of_int height ^".csv") in 
  let oc = open_out outputResultfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s" (finalPrint);   (* 写一些东西 *)
    close_out oc;


   
(*
  let pairs_tep = List.map (fun (a, b) -> ((showES a), (showES b))) pairs  in 

  let pairs_temp = List.map (fun (a, b) -> (Parser.es_p Lexer.token (Lexing.from_string a), Parser.es_p Lexer.token (Lexing.from_string b))) pairs_tep  in 
*)

(*

  let temp = List.map2 (fun (a,b, c) (e,f, g) -> a==e ) resultsChain resultsMirov in 
  
  let pass0 = List.filter (fun (a,b, c) -> a == true ) resultsChain in 
  let fail0 = List.filter (fun (a,b, c) -> a == false ) resultsChain in 



  print_string("=========Antichain=========\n");
  print_string ("Avg Time: "^(string_of_float((endTime0 -. startTimeStamp0) *. (float_of_int 1000)/. ((float_of_int sampleNum) *. float_of_int sampleNum)))^"\n" );
  let totalPstates = List.fold_left (fun acc (a,b) -> acc + b) 0 results0 in 

  print_string ("Avg PStates: "^(string_of_float(float_of_int(totalPstates) /. ((float_of_int sampleNum) *. float_of_int sampleNum)))^"\n" );
  (*
  print_string (List.fold_left (fun acc (a,b) -> acc ^"["^ string_of_bool a ^":"^string_of_int b ^"]\n") "" results0);
 *)
  
  print_string ("=========Antimirov========="^"\n");
  print_string ("Avg Time: "^(string_of_float((endTime -. startTimeStamp) *. (float_of_int 1000)/. ((float_of_int sampleNum) *. float_of_int sampleNum)))^"\n" );
  let totalInclusion = List.fold_left (fun acc (a,b) -> acc + b) 0 results in 
  print_string ("Avg Hypos: "^(string_of_float(float_of_int(totalInclusion) /. ((float_of_int sampleNum) *. float_of_int sampleNum)))^"\n" );
  (*
  print_string (List.fold_left (fun acc (a,b) -> acc ^"["^ string_of_bool a ^":"^string_of_int b ^"]\n") "" results);
  *)
  




  print_string ("\n**** Report ****\n"^
    "Height: "^ string_of_int height ^
    "\nPairs: "^ string_of_int (sampleNum*sampleNum) ^
    "\nHold: "^ string_of_int (List.length pass) ^", Fail: "^ string_of_int (List.length fail));
  
  print_string ("\nIncomplete: "^ string_of_int (List.fold_left(fun acc a -> if a then acc else acc + 1) 0 temp)^"\n");



  
*)

 