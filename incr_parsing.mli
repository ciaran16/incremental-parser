type ('tok, 'a) parser

type ('tok, 'a) prefix

type ('tok, 'a) infix

val pratt_parser :
  prefixes:('tok -> ('tok, 'a) prefix) -> infixes:('tok -> ('tok, 'a) infix) -> ('tok, 'a) parser

module Combinators : sig
  val eat : 'tok -> ('tok, 'tok) parser

  val any : ('tok, 'tok) parser

  val satisfy : ('tok -> 'a option) -> ('tok, 'a) parser

  val (<*>) : ('tok, 'a -> 'b) parser -> ('tok, 'a) parser -> ('tok, 'b) parser

  val (<$>) : ('a -> 'b) -> ('tok, 'a) parser -> ('tok, 'b) parser

  val ( *>) : ('tok, 'a) parser -> ('tok, 'b) parser -> ('tok, 'b) parser

  val (<* ) : ('tok, 'a) parser -> ('tok, 'b) parser -> ('tok, 'a) parser

  val fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser
end

module Prefix : sig
  val return : 'a -> ('tok, 'a) prefix

  val unary : ?prec:int -> ('a -> 'a) -> ('tok, 'a) prefix

  val custom : ('tok, 'a) parser -> ('tok, 'a) prefix
end

module Infix : sig
  val left : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val right : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val postfix : ?prec:int -> ('a -> 'a) -> ('tok, 'a) infix

  val unknown : ('tok, 'a) infix
end

module Non_incremental : sig
  val run : tokens:'tok Gadt_rope.t -> end_token:'tok -> ('tok, 'a) parser -> 'a
end

module Incremental : sig
  type ('tok, 'a) t

  val make : tokens:'tok Gadt_rope.t -> end_token:'tok -> ('tok, 'a) parser -> 'a * ('tok, 'a) t

  val update :
    start:int -> added:int -> removed:int -> tokens:'tok Gadt_rope.t -> ('tok, 'a) t ->
    'a * ('tok, 'a) t
end
