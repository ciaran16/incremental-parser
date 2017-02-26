module Make (Tag : Tagging.Tag) : sig
  type ('tok, 'a) parser

  type ('tok, 'a) prefix

  type ('tok, 'a) infix

  val pratt_parser : ?infixes:('tok -> ('tok, 'a) infix) -> ?prefixes:('tok -> ('tok, 'a) prefix) ->
    ?empty_prefix:('tok, 'a) prefix -> 'a Tag.t -> ('tok, 'a) parser

  module Combinators : sig
    val eat : 'tok -> ('tok, 'tok) parser

    val satisfy : ('tok -> 'a option) -> ('tok, 'a) parser

    val (<*>) : ('tok, 'a -> 'b) parser -> ('tok, 'a) parser -> ('tok, 'b) parser

    val (<$>) : ('a -> 'b) -> ('tok, 'a) parser -> ('tok, 'b) parser

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

  module Parse_tree : sig
    type 'a t

    val to_ast : 'a t -> 'a

    val length : 'a t -> int
  end

  module Non_incremental : sig
    val run : ('tok, 'a) parser -> lexer:'tok Incr_lexing.Lexer.t -> 'a Parse_tree.t
  end

  module Incremental : sig
    type ('tok, 'a) t

    val make : ('tok, 'a) parser -> lexer:'tok Incr_lexing.Lexer.t -> ('tok, 'a) t

    val parse_tree : ('tok, 'a) t -> 'a Parse_tree.t

    val update : start:int -> added:int -> removed:int -> lexer:'tok Incr_lexing.Lexer.t ->
      ('tok, 'a) t -> ('tok, 'a) t
  end
end
