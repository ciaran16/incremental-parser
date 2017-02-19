exception Out_of_bounds of string

val extract_exn : string -> 'a option -> 'a

module type S = sig
  type 'a t

  type 'a tree = 'a t

  val empty : 'a t

  val length : 'a t -> int

  val get : int -> 'a t -> 'a option

  val get_exn : int -> 'a t -> 'a

  val append : l:'a t -> r:'a t -> 'a t

  val concat : 'a t list -> 'a t

  val split : int -> 'a t -> ('a t * 'a t) option

  val split_exn : int -> 'a t -> 'a t * 'a t

  val sub : int -> int -> 'a t -> 'a t option

  val sub_exn : int -> int -> 'a t -> 'a t

  module Iterator : sig
    type 'a t

    (** Returns [None] if the tree has length 0. *)
    val start_at : int -> 'a tree -> 'a t option

    val is_at_end : 'a t -> bool

    (** Repeatedly returns the last item if called when at the end. *)
    val next : 'a t -> 'a * 'a t

    val skip : int -> 'a t -> 'a t
  end
end

module type Container = sig
  type 'a t

  val max_leaf_size : int

  (** Must take O(1) time. *)
  val length : 'a t -> int

  val get : 'a t -> int -> 'a

  val append : 'a t -> 'a t -> 'a t

  val sub : 'a t -> int -> int -> 'a t
end

module Make (C : Container) : sig
  include S

  val of_container : 'a C.t -> 'a t

  val of_container_list : 'a C.t list -> 'a t

  val flatten : 'a t -> 'a C.t list

  val flatten_map : ('a C.t -> 'b) -> 'a t -> 'b list

  val map_containers : ('a C.t -> 'b C.t) -> 'a t -> 'b t

  val repeat_to : int -> 'a t -> 'a t
  (** [repeat_to n t] creates a tree of length n consisting of t repeated.
      Useful for creating [make] function. Runs in O(log n) time. *)
end

module F_array : sig
  include S

  val of_array : 'a array -> 'a t

  val of_array_list : 'a array list -> 'a t

  val to_array : 'a t -> 'a array

  val of_list : 'a list -> 'a t

  val make : int -> 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Rope : sig
  include S

  val of_string : string -> char t

  val of_char : char -> char t

  val of_string_list : string list -> char t

  val to_string : char t -> string

  val make : int -> char -> char t

  val map : (char -> char) -> char t -> char t

  val printer : int -> Format.formatter -> char t -> unit

  val default_printer : Format.formatter -> char t -> unit
end

module One_tree : sig
  include S

  val singleton : 'a -> 'a t

  val of_list : 'a list -> 'a t

  val to_list : 'a t -> 'a list

  val make : int -> 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Quick_tree (Tree : S) : sig
  include S

  val of_tree : 'a Tree.t -> 'a t

  val to_tree : 'a t -> 'a Tree.t
end

module Zipped_trees (L : S) (R : S) : sig
  include S

  val create : l:'a L.t -> r:'b R.t -> ('a * 'b) t

  val left : ('a * 'b) t -> 'a L.t

  val right : ('a * 'b) t -> 'b R.t
end
