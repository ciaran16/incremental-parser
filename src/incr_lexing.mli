type 'tok lex_result

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

module Lexer : sig
  type 'tok t

  val make : (int -> 'tok lex_result * int) -> 'tok t

  val of_token_array : 'tok array -> 'tok t

  val of_ocamllex : (Lexing.lexbuf -> 'tok lex_result) -> make_lexbuf_at:(int -> Lexing.lexbuf) ->
    'tok t

  val pos : 'tok t -> int

  val next : 'tok t -> 'tok * int * 'tok t
  (** Returns a tuple of the form [(token, token_length, lexer)]. *)

  val peek : 'tok t -> 'tok

  val skip : int -> 'tok t -> 'tok t

  val move_to : int -> 'tok t -> 'tok t
end
