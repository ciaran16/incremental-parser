type 'a t

type (_, _) equal =
  | Equal : ('a, 'a) equal
  | Not_equal : ('a, 'b) equal

val fresh : unit -> 'a t

val compare : 'a t -> 'b t -> ('a, 'b) equal

val tag_count : unit -> int
