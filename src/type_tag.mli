type 'a t

type (_, _) maybe_equal =
  | Equal : ('a, 'a) maybe_equal
  | Not_equal : ('a, 'b) maybe_equal

val fresh : unit -> 'a t

val compare : 'a t -> 'b t -> ('a, 'b) maybe_equal

val tag_count : unit -> int
