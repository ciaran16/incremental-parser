module Rope = Gadt_rope.String_rope
module Iterator = Rope.Fast_iterator
module F_array = Gadt_rope.Functional_array

type 'tok lex_result

type 'tok lex_f = char Iterator.t -> 'tok lex_result * char Iterator.t

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result

module Non_incremental : sig
  val lex_all : char Rope.t -> end_token:'tok -> 'tok lex_f -> 'tok F_array.t
end

module Incremental : sig
  type 'tok t

  val make : char Rope.t -> end_token:'tok -> 'tok lex_f -> 'tok t
end
