type _ tag = ..

module type T = sig
  type a
  type _ tag += Tag : a tag
end

type 'a t = (module T with type a = 'a)

type (_, _) maybe_equal =
  | Equal : ('a, 'a) maybe_equal
  | Not_equal : ('a, 'b) maybe_equal

let count = ref 0

let tag_count () = !count

let create (type aa) () : aa t =
  count := !count + 1;
  (module struct
    type a = aa
    type _ tag += Tag : a tag
  end)

let compare (type a) (type b) ((module X) : a t) ((module Y) : b t) : (a, b) maybe_equal =
  match X.Tag with
  | Y.Tag -> Equal
  | _ -> Not_equal
