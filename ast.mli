(*----------------------------------------------------
---------------------DATA STRUCTURE-----------------
----------------------------------------------------*)
type terms = Var of string
           | Plus of terms * int
           | Minus of terms * int

(* We use a string to represent an single event *)
type event =  string 

(*E vent sequence *)
type es = Bot 
        | Emp 
        | Event of event
        | Cons of es * es
        | ESOr of es * es
        | Ttimes of es * terms
        | Omega of es

(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * int
          | Lt of terms * int
          | Eq of terms * int
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure



(*Effects*)
type effect = Effect of pure * es
          | Disj of effect * effect

type entilment = EE of effect * effect



