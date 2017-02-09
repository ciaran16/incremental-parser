module type S = sig
  type 'a t

  val length : 'a t -> int

  val append : 'a t -> 'a t -> 'a t

  val split_exn : int -> 'a t -> 'a t * 'a t
end

module type Container = sig
  type 'a t

  val max_leaf_size : int

  val length : 'a t -> int (** Must be O(1) *)

  val get : 'a t -> int -> 'a

  val append : 'a t -> 'a t -> 'a t

  val sub : 'a t -> int -> int -> 'a t
end

module Make (C : Container) : S

module Functional_array : sig
  include S

  val of_array : 'a array -> 'a t
end

module String_rope : sig
  include S

  val of_string : string -> char t
end
