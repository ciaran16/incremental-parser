open Json_lexer
open Incr_parsing
open Combinators

type json =
  | Obj of json_obj
  | Arr of json_arr
  | Str of string
  | Number of float
  | Bool of bool
  | Null

and json_obj =
  | Pair of string * json
  | Obj_node of json_obj * json_obj

and json_arr =
  | Value of json
  | Arr_node of json_arr * json_arr

let value = fix @@ fun value ->
  let name = satisfy (function STRING s -> Some s | _ -> None) in
  let pair = (fun n v -> Pair (n, v)) <$> name <*> eat COLON *> value in
  let wrap_value = (fun v -> Value v) <$> value in
  let prefixes = function
    | OBJ_START ->   Prefix.list pair (fun l r -> Obj_node (l, r))
                       ~sep:COMMA ~stop:OBJ_END ~wrap:(fun o -> Obj o)
    | ARRAY_START -> Prefix.list wrap_value (fun l r -> Arr_node (l, r))
                       ~sep:COMMA ~stop:ARRAY_END ~wrap:(fun a -> Arr a)
    | BOOL b ->      Prefix.return (Bool b)
    | NULL ->        Prefix.return Null
    | STRING s ->    Prefix.return (Str s)
    | NUMBER n ->    Prefix.return (Number n)
    | _ ->           Prefix.unknown
  in
  pratt_parser prefixes
