type (_, _) equal =
  | Equal : ('a, 'a) equal
  | Not_equal : ('a, 'b) equal

module Lift (T : sig type 'a t end) : sig
  val f : ('a, 'b) equal -> ('a T.t, 'b T.t) equal
end

module type Tag = sig
  type 'a t

  val tags_equal : 'a t -> 'b t -> ('a, 'b) equal
end
