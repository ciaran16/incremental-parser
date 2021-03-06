type 'tok t

type 'tok lex_result

val make : (int -> 'tok * int) -> 'tok t

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

val handle_errors : 'tok lex_result t -> 'tok t

val of_token_array : 'tok array -> 'tok t

val of_token_and_length_array : ('tok * int) array -> 'tok t

val of_ocamllex : (Lexing.lexbuf -> 'tok) -> make_lexbuf_at:(int -> Lexing.lexbuf) -> 'tok t

val of_ocamllex_and_string : (Lexing.lexbuf -> 'tok) -> string -> 'tok t

val pos : 'tok t -> int

val next : 'tok t -> 'tok * int * 'tok t
(** Returns a tuple of the form [(token, token_length, lexer)]. *)

val peek : 'tok t -> 'tok

val skip : int -> 'tok t -> 'tok t

val move_to : int -> 'tok t -> 'tok t

val verbose : bool ref
