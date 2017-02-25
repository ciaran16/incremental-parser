{
open Incr_lexing

type token =
  | END
  | INT of int
  | PLUS | MINUS | TIMES | DIVIDE | POW | FACT
  | BOOL of bool
  | EQUAL | AND | OR
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
  | digit+ as int_string { token @@ INT (int_of_string int_string) }
  | '+' { token PLUS }
  | '-' { token MINUS }
  | '*' { token TIMES }
  | '/' { token DIVIDE }
  | '^' { token POW }
  | '!' { token FACT }
  | "true" { token (BOOL true) }
  | "false" { token (BOOL false) }
  | '=' { token EQUAL }
  | "&&" { token AND }
  | "||" { token OR }
  | '(' { token PAREN_L }
  | ')' { token PAREN_R }
  | "if" { token IF }
  | "then" { token THEN }
  | "else" { token ELSE }
  | "let" { token LET }
  | "in" { token IN }
  | identifier as s { token (IDENT s) }
  | eof { token END }
  | _ { error ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }
