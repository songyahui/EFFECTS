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

let alphabet = ["A"; "B"; "C"; "d"; "e"; "f"; "g"; "h"; "I"; "J"; "K"; "L"; "M"; "N"]

let height = 2;;
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

let getFst (a, b) = a ;;

let rec genES (num:int) (acc:es list): es list= 
  if num = 0 then acc
  else 
  (let one =  regexGen height sigma in 
  genES (num - 1) (append acc [one] );
  ) (**)
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
  
  
  let outputfile = (Sys.getcwd ()^ "/" ^ "Testing/regex.dat") in
  
  
  let ess = genES sampleNum  [] in
  let pairs = cartesian ess ess (*[(Cons(Event "B", Cons(Event "B", Kleene(Event "B"))),Cons (Kleene(Event "B"), Cons(Event "B",Event "B") ))] *)in 
  
  let dataset' = List.fold_left (fun acc (lhs, rhs) -> acc ^ showEntailmentESReg lhs rhs ^"\n") "" pairs in 
  let dataset = List.fold_left (fun acc (lhs, rhs) -> acc ^ showEntailmentES lhs rhs ^"\n") "" pairs in 
  let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s" (dataset'^"\n"^dataset);   (* 写一些东西 *)
    close_out oc;

    
  let startTimeStamp0 = Sys.time() in
  let results0 = List.map (fun (lhs, rhs) -> 
    let re = RegToNfa.antichain (showESReg lhs) (showESReg rhs)  in 
    (*print_string (string_of_bool (getFst re)); *)
    re
  )
  pairs in 
  let endTime0 = Sys.time() in 
   

  let pairs_temp = List.map (fun (a, b) -> ((showES a), (showES b))) pairs  in 

  let startTimeStamp = Sys.time() in
  let results = List.map (fun (lhs, rhs) -> (Antimirov.antimirov_shell lhs rhs )) pairs_temp in 
  let endTime = Sys.time() in 

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
  let temp = List.map2 (fun (a,b) (c,d) -> a==c ) results0 results in 
  
  let pass = List.filter (fun (a,b) -> a == true ) results0 in 
  let fail = List.filter (fun (a,b) -> a == false ) results0 in 

  



  print_string ("\n**** Report ****\n"^
    "Height: "^ string_of_int height ^
    "\nPairs: "^ string_of_int (sampleNum*sampleNum) ^
    "\nHold: "^ string_of_int (List.length pass) ^", Fail: "^ string_of_int (List.length fail));
  
  print_string ("\nIncomplete: "^ string_of_int (List.fold_left(fun acc a -> if a then acc else acc + 1) 0 temp)^"\n");



  


 