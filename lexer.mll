{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
let int =  '-'? ['0'-'9'] ['0'-'9']*

(* part 2 *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* part 3 *)
let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n" 
let id = ['a'-'v' 'x'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule token = parse
| white    { token lexbuf }
| newline  { next_line lexbuf; token lexbuf }
| "emp" { EMPTY }
| "int" {INTT} 
| "bool" {BOOLT}
| "void" {VOIDT}
| "event" {EVENTKEY}
| "assert" {ASSERTKEY}
| "return" {RETURN}
| "|-" {ENTIL}
| "TRUE" { TRUE }
| "FALSE" { FALSE }
| "if" {IF}
| "else" {ELSE}
| "require" {REQUIRE}
| "ensure" {ENSURE}
| "include" {INCLUDE}
| int      { INTE (int_of_string (Lexing.lexeme lexbuf)) }
| "true" { TRUEE (bool_of_string (Lexing.lexeme lexbuf))}
| "false" { FALSEE (bool_of_string (Lexing.lexeme lexbuf))}
| '"'      { read_string (Buffer.create 17) lexbuf }
| ['A'-'T' 'V' 'W' 'Y' 'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as str { EVENT str }
| id as str { VAR str }
| ">=" {GTEQ}
| "<=" {LTEQ}
| '>' {GT}
| '<' {LT}
| '=' {EQ}
| '^' { POWER }
(*| 'w' { OMEGA }*)
| '|' { CHOICE }
| '.' { CONCAT }
| '"' { read_string (Buffer.create 17) lexbuf }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBrackets }
| ']' { RBrackets }
| '{' { LBRACK  }
| '}' { RBRACK }
| ',' { COMMA }
| ';' { SIMI }
| '+' { PLUS }
| '-' { MINUS }
| '#' { SHARP }
| '_' {UNDERLINE}
| '*' {KLEENE}
| '~' {NEGATION}
| "/*" {LSPEC}
| "*/" {RSPEC}
| "\\/" {DISJ}
| "/\\" {CONJ}
| "==" {EQEQ}
| ">=" {GTEQ}
| "<=" {LTEQ}
| "<>" {FUTURE}  
| "[]" {GLOBAL}
| "->" {IMPLY}
| '!' {LTLNOT}
| 'X' {NEXT}
| 'U' {UNTIL}
| "&&" {LILAND}
| "||" {LILOR}
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }

(* part 5 *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
