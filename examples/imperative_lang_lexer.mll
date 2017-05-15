{
type token =
  | END
  | INT of int
  | PLUS | MINUS | TIMES | DIVIDE | POW | FACT
  | BOOL of bool
  | EQUALS | AND | OR
  | IDENT of string
  | L_PAREN | R_PAREN
  | SEMICOLON
  | L_BRACE | R_BRACE
  | ASSIGN
  | IF | ELSE
  | WHILE
  | FOR
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
  | "==" { EQUALS }
  | "&&" { AND }
  | "||" { OR }
  | '(' { L_PAREN }
  | ')' { R_PAREN }
  | identifier as s { IDENT s }
  | ';' { SEMICOLON }
  | '{' { L_BRACE }
  | '}' { R_BRACE }
  | "=" { ASSIGN }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | eof { END }
  | _ { failwith ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }
