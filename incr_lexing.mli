module Iterator = Gadt_rope.Iterator

type 'tok lex_result

type ('char, 'tok) lex_f = 'char Iterator.t -> 'tok lex_result * 'char Iterator.t

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

module Non_incremental : sig
  val lex_all : 'char Gadt_rope.t -> end_token:'tok -> ('char, 'tok) lex_f -> 'tok Gadt_rope.t
end

module Incremental : sig
  type ('char, 'tok) t

  val make : 'char Gadt_rope.t -> end_token:'tok -> ('char, 'tok) lex_f -> ('char, 'tok) t
end
