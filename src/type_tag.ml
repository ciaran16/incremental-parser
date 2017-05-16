type _ tag = ..

module type T = sig
  type a
  type _ tag += Tag : a tag
end

type 'a t = (module T with type a = 'a)

let count = ref 0

let tag_count () = !count

let fresh (type aa) () : aa t =
  count := !count + 1;
  (module struct
    type a = aa
    type _ tag += Tag : a tag
  end)

type (_, _) maybe_equal =
  | Equal : ('a, 'a) maybe_equal
  | Not_equal : ('a, 'b) maybe_equal

let compare : type a b. a t -> b t -> (a, b) maybe_equal = fun (module X) (module Y) ->
  match X.Tag with
  | Y.Tag -> Equal
  | _ -> Not_equal
