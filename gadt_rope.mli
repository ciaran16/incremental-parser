type 'a t

val length : 'a t -> int

val empty : 'a t

(** Takes O(n) time. *)
val of_list : 'a list -> 'a t

val of_string : string -> char t

val get : int -> 'a t -> 'a option

val insert_exn : int -> 'a -> 'a t -> 'a t

val delete_exn : int -> 'a t -> 'a t

module Iterator : sig
  type 'a t

  (* Has amortized cost O(1) when iterating through contiguous items. *)
  val next : 'a t -> 'a option * 'a t

  val take : int -> 'a t -> 'a list * 'a t

  val drop : int -> 'a t -> 'a t
end

val iterator : 'a t -> 'a Iterator.t

val iterator_at : int -> 'a t -> 'a Iterator.t
