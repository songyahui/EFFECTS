exception Error

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


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.entilment)