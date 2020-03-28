(*----------------------------------------------------
---------------------DATA STRUCTURE-----------------
----------------------------------------------------*)             

type terms = Var of string
           | Number of int
           | Plus of terms * terms
           | Minus of terms * terms 

(* We use a string to represent an single event *)
type event =  string 

(*E vent sequence *)
type es = Bot 
        | Emp 
        | Event of event
        | Cons of es * es
        | ESOr of es * es
        | Ttimes of es * terms
        | Kleene of es
        | Omega of es
        | Range of (es list)
        | Underline

(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure

(*Effects*)
type effect = 
            Effect of pure * es
          | Disj of effect * effect


type entilment = EE of effect * effect

type spec = PrePost of effect * effect

type _type = INT | FLOAT | BOOL | VOID

type mn = string
type var = string 
type includ = string 

type expression = Unit 
          | Return
          | Integer of int
          | Bool of bool
          | Float of float
          | String of string
          | Variable of var
          | LocalDel of _type * var * expression 
          | Call of mn * expression list 
          | Assign of var * expression
          | Seq of expression * expression
          | EventRaise of event
          | IfElse of expression * expression * expression
          | Cond of expression * expression * string
          | BinOp of expression * expression * string
          | Assertion of effect

type param  = (_type * var) list

type meth = Meth of _type * mn * param * spec * expression

type declare = Include of string | Method of meth

type program = declare list





