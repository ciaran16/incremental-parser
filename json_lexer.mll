{
open Incr_lexing

type token =
  | END
  | OBJ_START | OBJ_END
  | ARRAY_START | ARRAY_END
  | COLON
  | COMMA
  | BOOL of bool
  | NULL
  | NUMBER of float
  | STRING of string
}

let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['+' '-'] digit+
let number = '-'? ('0' | (['1'-'9'] digit*)) frac? exp?
let not_num = (digit | ['.' 'e' 'E' '+' '-'])+

rule lex = parse
  | ' ' | '\t' | '\n' | '\r' { lex lexbuf }
  | number { token @@ NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' { lex_string (Buffer.create 16) lexbuf }
  | '{' { token OBJ_START }
  | '}' { token OBJ_END }
  | ':' { token COLON }
  | '[' { token ARRAY_START }
  | ']' { token ARRAY_END }
  | ',' { token COMMA }
  | "true" { token (BOOL true) }
  | "false" { token (BOOL false) }
  | "null" { token NULL }
  | eof { token END }
  | ['a'-'z' 'A'-'Z']+ { error ("Unknown identifier '" ^ Lexing.lexeme lexbuf ^ "'.") }
  | not_num { error ("Incorrectly formatted number '" ^ Lexing.lexeme lexbuf ^ "'.") }
  | _ { error ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }

and lex_string b = parse
  | '"' { token @@ STRING (Buffer.contents b) }
  | '\\' 'b' { Buffer.add_char b '\b'; lex_string b lexbuf }
  | '\\' 'f' { Buffer.add_char b '\012'; lex_string b lexbuf }
  | '\\' 'n' { Buffer.add_char b '\n'; lex_string b lexbuf }
  | '\\' 'r' { Buffer.add_char b '\r'; lex_string b lexbuf }
  | '\\' 't' { Buffer.add_char b '\t'; lex_string b lexbuf }
  | '\\' 'u' _ _ _ _ { lex_string b lexbuf } (* Unicode escape sequences are skipped for now. *)
  | '\\' (_ as c) { Buffer.add_char b c; lex_string b lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string b (Lexing.lexeme lexbuf); lex_string b lexbuf }
  | eof | _ { error "Unterminated string." ~token:(STRING (Buffer.contents b)) }
