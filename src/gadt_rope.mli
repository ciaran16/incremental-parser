type t

type rope = t

val empty : t

val of_string : string -> t

val to_string : t -> string

val length : t -> int

val get : int -> t -> char option

val get_exn : int -> t -> char

val append : l:t -> r:t -> t

val (<^>) : t -> t -> t

val split : int -> t -> (t * t) option

val split_exn : int -> t -> t * t

val sub : int -> int -> t -> t option

val sub_exn : int -> int -> t -> t

val concat : t list -> t

module Iterator : sig
  type iter

  (** Returns [None] if the tree has length 0. *)
  val start_at : int -> t -> iter option

  val is_at_end : iter -> bool

  (** Repeatedly returns the last item if called when at the end. *)
  val next : iter -> char * iter

  val skip : int -> iter -> iter
end

val printer : int -> Format.formatter -> t -> unit

val default_printer : Format.formatter -> t -> unit
