open Gadt_tree
open Json_lexer

type json =
  | Obj of (string * json) F_array.t
  | Arr of json F_array.t
  | Str of string
  | Number of float
  | Bool of bool
  | Null

module Tag = struct
  open Tagging

  type _ t =
    | Value : json t
    | Pair : (string * json) t

  let tags_equal : type a b. a t -> b t -> (a, b) equal = fun a_tag b_tag ->
    match a_tag, b_tag with
    | Value, Value -> Equal
    | Pair, Pair -> Equal
    | _ -> Not_equal
end

module Incr_parsing = Incr_parsing.Make (Tag)
open Incr_parsing
open Combinators

let value = fix @@ fun value ->
  let name = satisfy (function STRING s -> Some s | _ -> None) in
  let pair = (fun n v -> (n, v)) <$> name <*> eat COLON *> value in
  let prefixes = function
  (*| OBJ_START ->   Prefix.list pair ~sep:COMMA ~close:OBJ_END (fun o -> Obj o)
    | ARRAY_START -> Prefix.list value ~sep:COMMA ~close:ARRAY_END (fun a -> Arr a) *)
    | BOOL b ->      Prefix.return (Bool b)
    | NULL ->        Prefix.return Null
    | STRING s ->    Prefix.return (Str s)
    | NUMBER n ->    Prefix.return (Number n)
    | _ ->           Prefix.unknown
  in
  pratt_parser Tag.Value ~prefixes
