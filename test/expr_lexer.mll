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
let frac = '.' digit+
let exp = ['e' 'E'] ['+' '-'] digit+
let number = '-'? ('0' | (['1'-'9'] digit*)) frac? exp?
let not_num = (digit | ['.' 'e' 'E' '+' '-'])+

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
  | eof { token END }
  | (['a'-'z' 'A'-'Z' '_'] | digit)+ as s { error ("Unknown identifier '" ^ s ^ "'.") }
  | _ { error ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }
