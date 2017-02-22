type 'tok lex_result

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

type 'tok lexer = int -> 'tok lex_result * int

val of_list : 'tok list -> 'tok lexer

module Lexer : sig
  type 'tok t

  val make : 'tok lexer -> 'tok t

  val make_at : int -> 'tok lexer -> 'tok t

  val pos : 'tok t -> int

  val next : 'tok t -> 'tok * 'tok t

  val peek : 'tok t -> 'tok

  val skip : int -> 'tok t -> 'tok t

  val move_to : int -> 'tok t -> 'tok t
end
