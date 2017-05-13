open Json_lexer
open Incr_parsing
open Combinators

type json =
  | Obj of (string * json) tree option
  | Arr of json tree option
  | String_lit of string
  | Int_lit of int
  | Float_lit of float
  | Bool_lit of bool
  | Null

let value = fix @@ fun value ->
  let key = satisfy (function STRING s -> Some s | _ -> None) in
  let pair = (fun k v -> (k, v)) <$> key <*> eat COLON *> value in
  let pair_list = tree_of pair ~sep:COMMA ~close:OBJ_END in
  let value_list = tree_of value ~sep:COMMA ~close:ARRAY_END in
  let prefixes = function
    | OBJ_START ->   Prefix.custom (pair_list >>| fun l -> Obj l)
    | ARRAY_START -> Prefix.custom (value_list >>| fun l -> Arr l)
    | STRING s ->    Prefix.return (String_lit s)
    | INT n ->       Prefix.return (Int_lit n)
    | FLOAT f ->     Prefix.return (Float_lit f)
    | BOOL b ->      Prefix.return (Bool_lit b)
    | NULL ->        Prefix.return Null
    | _ ->           Prefix.unknown
  in
  pratt_parser ~prefixes ()
