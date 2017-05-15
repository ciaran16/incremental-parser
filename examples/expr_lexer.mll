{
type token =
  | END
  | INT of int
  | PLUS | MINUS | TIMES | DIVIDE | POW | FACT
  | BOOL of bool
  | EQUALS | AND | OR
  | PAREN_L | PAREN_R
  | IF | THEN | ELSE
  | LET | IN
  | IDENT of string
}

let digit = ['0'-'9']
let ident_first = ['a'-'z' 'A'-'Z' '_']
let identifier = ident_first (ident_first | digit)*

rule lex = parse
  | ' ' | '\t' | '\n' | '\r' { lex lexbuf }
  | digit+ as int_string { INT (int_of_string int_string) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '^' { POW }
  | '!' { FACT }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | '=' { EQUALS }
  | "&&" { AND }
  | "||" { OR }
  | '(' { PAREN_L }
  | ')' { PAREN_R }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | identifier as s { IDENT s }
  | eof { END }
  | _ { failwith ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }
