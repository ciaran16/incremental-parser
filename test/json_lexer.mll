{
type token =
  | END
  | OBJ_START | OBJ_END
  | ARRAY_START | ARRAY_END
  | COLON
  | COMMA
  | BOOL of bool
  | NULL
  | FLOAT of float
  | INT of int
  | STRING of string
}

let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['+' '-']? digit+
let int = '-'? ('0' | (['1'-'9'] digit*))
let float = int frac? exp?

rule lex = parse
  | ' ' | '\t' | '\n' | '\r' { lex lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' { lex_string (Buffer.create 16) lexbuf }
  | '{' { OBJ_START }
  | '}' { OBJ_END }
  | ':' { COLON }
  | '[' { ARRAY_START }
  | ']' { ARRAY_END }
  | ',' { COMMA }
  | "true" { (BOOL true) }
  | "false" { (BOOL false) }
  | "null" { NULL }
  | eof { END }
  | _ { failwith ("Unknown character '" ^ Lexing.lexeme lexbuf ^ "'.") }

and lex_string b = parse
  | '"' { STRING (Buffer.contents b) }
  | '\\' 'b' { Buffer.add_char b '\b'; lex_string b lexbuf }
  | '\\' 'f' { Buffer.add_char b '\012'; lex_string b lexbuf }
  | '\\' 'n' { Buffer.add_char b '\n'; lex_string b lexbuf }
  | '\\' 'r' { Buffer.add_char b '\r'; lex_string b lexbuf }
  | '\\' 't' { Buffer.add_char b '\t'; lex_string b lexbuf }
  | '\\' 'u' _ _ _ _ { lex_string b lexbuf } (* Unicode escape sequences are skipped for now. *)
  | '\\' (_ as c) { Buffer.add_char b c; lex_string b lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string b (Lexing.lexeme lexbuf); lex_string b lexbuf }
  | eof | _ { failwith "Unterminated string." }
