type 'tok lex_result

type ('char, 'tok) lex_f = 'char Rope.Iterator.t -> 'tok lex_result * 'char Rope.Iterator.t

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

module Non_incremental : sig
  val lex_all : 'char Rope.t -> end_token:'tok -> ('char, 'tok) lex_f -> 'tok Rope.t
end

module Incremental : sig
  type ('char, 'tok) t

  val make : 'char Rope.t -> end_token:'tok -> ('char, 'tok) lex_f -> ('char, 'tok) t
end
