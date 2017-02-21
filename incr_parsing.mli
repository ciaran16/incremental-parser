open Gadt_tree

type ('tok, 'a) parser

type ('tok, 'a) prefix

type ('tok, 'a) infix

module Type : sig
  type ('a, 'b) is_equal

  val equal : ('a, 'a) is_equal

  val not_equal : ('a, 'b) is_equal
end

module Combinators : sig
  val pratt_parser :
    ?empty_prefix:('tok, 'a) prefix ->
    ?infixes:('tok -> ('tok, 'a) infix) ->
    ('tok -> ('tok, 'a) prefix) ->
    ('tok, 'a) parser

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

  val unknown : ('tok, 'a) prefix
end

module Infix : sig
  val left : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val right : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val postfix : ?prec:int -> ('a -> 'a) -> ('tok, 'a) infix

  val unknown : ('tok, 'a) infix
end

module Non_incremental : sig
  val run : tokens:'tok F_array.t -> ('tok, 'a) parser -> 'a
end

module Incremental : sig
  type ('tok, 'a) t

  val make : tokens:'tok F_array.t -> ('tok, 'a) parser -> 'a * ('tok, 'a) t

  val update : start:int -> added:int -> removed:int -> tokens:'tok F_array.t -> ('tok, 'a) t ->
    'a * ('tok, 'a) t
end
