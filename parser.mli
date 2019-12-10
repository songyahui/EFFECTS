
(* The type of tokens. *)

type token = 
  | VOIDT
  | VAR of (string)
  | TRUEE of (bool)
  | TRUE
  | TESTPRO
  | STRING of (string)
  | SIMI
  | RSPEC
  | RPAR
  | REQUIRE
  | RBRACK
  | POWER
  | PLUS
  | OMEGA
  | MINUS
  | LT
  | LSPEC
  | LPAR
  | LBRACK
  | INTT
  | INTE of (int)
  | IF
  | GT
  | FLOATT
  | FLOATE of (float)
  | FALSEE of (bool)
  | FALSE
  | EVENTKEY
  | EVENT of (string)
  | EQ
  | EOF
  | ENTIL
  | ENSURE
  | EMPTY
  | ELSE
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
