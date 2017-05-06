type ('tok, 'a) parser

type ('tok, 'a) prefix

type ('tok, 'a) infix

val pratt_parser : ?prefixes:('tok -> ('tok, 'a) prefix) -> ?empty_prefix:('tok, 'a) prefix ->
  ?infixes:('tok -> ('tok, 'a) infix) -> ?tag:'a Type_tag.t -> unit -> ('tok, 'a) parser
(** The functions [prefixes] and [infixes] should be pure.
    This function should not be called during parsing. *)

module Combinators : sig
  val eat : 'tok -> ('tok, 'tok) parser

  val satisfy : ?on_error:'a -> ('tok -> 'a option) -> ('tok, 'a) parser

  val (<*>) : ('tok, 'a -> 'b) parser -> ('tok, 'a) parser -> ('tok, 'b) parser

  val (<$>) : ('a -> 'b) -> ('tok, 'a) parser -> ('tok, 'b) parser

  val (>>|) : ('tok, 'a) parser -> ('a -> 'b) -> ('tok, 'b) parser

  val ( *>) : ('tok, 'a) parser -> ('tok, 'b) parser -> ('tok, 'b) parser

  val (<* ) : ('tok, 'a) parser -> ('tok, 'b) parser -> ('tok, 'a) parser

  val fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser
end

(** A higher precedence is given by a lower number.
    A precendece of 1 is higher than a precedence of 2. *)

module Prefix : sig
  val return : 'a -> ('tok, 'a) prefix

  val unary : ?prec:int -> ('a -> 'a) -> ('tok, 'a) prefix
  (** The default precedence is -1 (a very high precedence), as prefix operators normally have
      higher precedence than infix operators. *)

  val custom : ('tok, 'a) parser -> ('tok, 'a) prefix

  val unknown : ('tok, 'a) prefix
end

module Infix : sig
  val left : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val right : int -> ('a -> 'a -> 'a) -> ('tok, 'a) infix

  val postfix : ?prec:int -> ('a -> 'a) -> ('tok, 'a) infix
  (** The default precedence is -2 (a very high precedence), as postfix operators normally have
      higher precedence than prefix and infix operators. *)

  val unknown : ('tok, 'a) infix
end

module Incremental : sig
  type ('tok, 'a) t

  val make : ('tok, 'a) parser -> lexer:'tok Incr_lexing.Lexer.t -> ('tok, 'a) t

  val update : start:int -> added:int -> removed:int -> lexer:'tok Incr_lexing.Lexer.t ->
    ('tok, 'a) t -> ('tok, 'a) t

  val to_ast : ('tok, 'a) t -> 'a

  val length : ('tok, 'a) t -> int
end
