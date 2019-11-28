
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TRUE
  | SPACES
  | RPAR
  | POWER
  | PLUS
  | OMEGA
  | NUM of (int)
  | MINUS
  | LT
  | LPAR
  | GT
  | FALSE
  | EVENT of (string)
  | EQ
  | EOF
  | ENTIL
  | EMPTY
  | DISJ
  | CONJ
  | CONCAT
  | CHOICE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.entilment)
