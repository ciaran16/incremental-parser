module type S = sig
  type 'a t

  type 'a rope = 'a t

  val max_leaf_size : int

  val length : 'a t -> int

  val empty : 'a t

  val append : 'a t -> 'a t -> 'a t

  val concat : 'a t list -> 'a t

  val split_exn : int -> 'a t -> 'a t * 'a t

  module Fast_iterator : sig
    type 'a t

    (** Returns [None] if the rope has length 0. *)
    val start_at : int -> 'a rope -> 'a t option

    val is_at_end : 'a t -> bool

    val next : 'a t -> 'a * 'a t

    val skip : int -> 'a t -> 'a t
  end
end

module type Container = sig
  type 'a t

  val max_leaf_size : int

  val length : 'a t -> int (** Must take O(1) time. *)

  val get : 'a t -> int -> 'a

  val append : 'a t -> 'a t -> 'a t

  val sub : 'a t -> int -> int -> 'a t
end

module Make (C : Container) : sig
  include S

  val of_container : 'a C.t -> 'a t

  val flatten : 'a t -> 'a C.t list
end

module Functional_array : sig
  include S

  val of_array : 'a array -> 'a t

  val to_array : 'a t -> 'a array
end

module String_rope : sig
  include S

  val of_string : string -> char t

  val to_string : char t -> string
end

module One_rope : sig
  include S

  val of_list : 'a list -> 'a t

  val to_list : 'a t -> 'a list
end
