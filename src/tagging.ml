type (_, _) equal =
  | Equal : ('a, 'a) equal
  | Not_equal : ('a, 'b) equal

module Lift (T : sig type 'a t end) = struct
  let f : type a b. (a, b) equal -> (a T.t, b T.t) equal = function
    | Equal -> Equal
    | Not_equal -> Not_equal
end

module type Tag = sig
  type 'a t

  val tags_equal : 'a t -> 'b t -> ('a, 'b) equal
end
