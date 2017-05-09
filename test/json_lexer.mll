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

let rev_concat = function
| [] -> ""
| [s] -> s
| l -> List.rev l |> String.concat ""
}

let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['+' '-']? digit+
let int = '-'? ('0' | (['1'-'9'] digit*))
let float = int frac? exp?

rule lex = parse
  | ' ' | '\t' | '\n' | '\r' { lex lexbuf }
  | int as n { INT (int_of_string n) }
  | float as f { FLOAT (float_of_string f) }
  | '"' { lex_string [] lexbuf }
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

and lex_string acc = parse
  | '"' { STRING (rev_concat acc) }
  | '\\' 'b' { lex_string ("\b"::acc) lexbuf }
  | '\\' 'f' { lex_string ("\012"::acc) lexbuf }
  | '\\' 'n' { lex_string ("\n"::acc) lexbuf  }
  | '\\' 'r' { lex_string ("\r"::acc) lexbuf  }
  | '\\' 't' { lex_string ("\t"::acc) lexbuf  }
  | '\\' 'u' _ _ _ _ { lex_string acc lexbuf } (* Unicode escape sequences are skipped for now. *)
  | '\\' (_ as c) { lex_string (String.make 1 c :: acc) lexbuf  }
  | [^ '"' '\\']+ as s { lex_string (s::acc) lexbuf }
  | eof | _ { failwith "Unterminated string." }
