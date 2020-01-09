open Random
open Ast
open Pretty
open Printf
open List
open Rewriting

type op = OpCon | OpStar | OpUinon

let showOp (o:op) :string = 
  match o with 
    OpCon  -> "OpCon\n"
  | OpStar -> "OpStar\n"
  | OpUinon-> "OpUinon\n"
  ;;

let alphabet = ["A"; "B"; "c"; "d"; "e"; "f"; "g"; "h"; "I"; "J"; "K"; "L"; "M"; "N"]

let height = 3;;
let sigma = 2;;
let sampleNum = 1;;

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
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
;;

let getFst (a, b) = a ;;



let main =
  print_string ("song\n");
  let line = "TRUE /\\ ((A^*).(B^*).(B.B^*)^*) |- TRUE /\\ ((A^*).(B^*).(B.B^*)^*) \n" in
  let EE (Effect (p1,lhs), Effect (p2,rhs)) = Parser.ee Lexer.token (Lexing.from_string line) in
  (*let result = printReport (Effect (p1,lhs)) (Effect (p2,rhs)) in *)
  let result = string_of_bool (getFst (Rewriting.antimirov (Effect (p1,lhs)) (Effect (p2,rhs)) [])) in 
  print_string(result);;
  (**)

  
  
  (*let outputfile = (Sys.getcwd ()^ "/" ^ "Testing/regex.dat") in
  
  let rec genES (num:int) (acc:es list): es list= 
    if num = 0 then acc
    else 
    (let one =  regexGen height sigma in 
    genES (num - 1) (append acc [one] );
    ) (**)
  in 
  let ess = genES sampleNum  [] in
  let pairs = cartesian ess ess in 
  let dataset = List.fold_left (fun acc (lhs, rhs) -> acc ^ showEntailmentESReg lhs rhs ^"\n") "" pairs in 
  let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
    fprintf oc "%s" dataset;   (* 写一些东西 *)
    close_out oc;
  
  let results = List.map (fun (lhs, rhs) -> (Rewriting.antimirov lhs rhs [])) pairs in 

  print_string (List.fold_left (fun acc a -> acc ^ string_of_bool a ^"\n") "" results);;
    
    *)
