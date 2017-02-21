type 'tok lex_result

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

type 'tok lex_f = int -> 'tok lex_result * int

module Lexer : sig
  type 'tok t

  val start_at : int -> 'tok lex_f -> 'tok t

  val next : 'tok t -> 'tok * 'tok t

  val peek : 'tok t -> 'tok

  val skip : int -> 'tok t -> 'tok t

  val pos : 'tok t -> int
end
