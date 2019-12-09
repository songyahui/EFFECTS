{ open Parser }

rule token = parse
| "program" {TESTPRO}
| "|-" {ENTIL}
| [' ']* {SPACES} 
| "TRUE" { TRUE }
| "FALSE" { FALSE }
| ['0'-'9'] ['0'-'9']* as num {NUM (int_of_string num)}
| ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as str { EVENT str }
| ['a'-'v' 'x'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as str { VAR str }
| "Emp" { EMPTY }
| '>' {GT}
| '<' {LT}
| '=' {EQ}
| '^' { POWER }
| 'w' { OMEGA }
| '|' { CHOICE }
| '.' { CONCAT }
| '(' { LPAR }
| ')' { RPAR }
| '+' { PLUS }
| '-' { MINUS }
| "\\/" {DISJ}
| "/\\" {CONJ}
| eof { EOF }

