
(* The type of tokens. *)

type token = 
  | VOIDT
  | VAR of (string)
  | TRUEE of (bool)
  | TRUE
  | STRING of (string)
  | SIMI
  | SHARP
  | RSPEC
  | RPAR
  | REQUIRE
  | RBRACK
  | POWER
  | PLUS
  | OMEGA
  | MINUS
  | LTEQ
  | LT
  | LSPEC
  | LPAR
  | LBRACK
  | INTT
  | INTE of (int)
  | INCLUDE
  | IF
  | GTEQ
  | GT
  | FLOATT
  | FLOATE of (float)
  | FALSEE of (bool)
  | FALSE
  | EVENTKEY
  | EVENT of (string)
  | EQEQ
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

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

val ee: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.entilment)
