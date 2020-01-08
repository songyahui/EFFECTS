open Random
open Ast
open Pretty

type op = OpCon | OpStar | OpUinon

let showOp (o:op) :string = 
  match o with 
    OpCon  -> "OpCon\n"
  | OpStar -> "OpStar\n"
  | OpUinon-> "OpUinon\n"
  ;;

let alphabet = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"]

let height = 4;;
let sigma = 2;;

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
    let num = Random.int  3 in 
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


let main =
  let rec genOne (num:int) = 
    if num = 0 then Unit
    else 
    (let one =  regexGen height sigma in 
    print_string (showESReg (normalES one TRUE) ^"\n");
    genOne (num - 1);
    )
  in genOne 30;;
