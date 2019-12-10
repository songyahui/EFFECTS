
(* The type of tokens. *)

type token = 
  | VOIDT
  | VAR of (string)
  | TRUE
  | TESTPRO
  | STRING of (string)
  | RPAR
  | RBRACK
  | POWER
  | PLUS
  | OMEGA
  | MINUS
  | LT
  | LPAR
  | LBRACK
  | INTT
  | INTE of (int)
  | GT
  | FLOATT
  | FLOATE of (float)
  | FALSE
  | EVENT of (string)
  | EQ
  | EOF
  | ENTIL
  | EMPTY
  | DISJ
  | CONJ
  | CONCAT
  | COMMA
  | CHOICE
  | BOOLT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val meth: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.meth)

val ee: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.entilment)
