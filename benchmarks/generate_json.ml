open Json_lexer
open Json_rd_parser

type spec = unit -> json

type list_type = int -> ?upto:int -> spec -> spec

(* Inclusive of low and high. *)
let random_int low high =
  if low = high then low
  else low + Random.int (high - low + 1)

let unquoted_string () =
  let allowed_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz        " in
  let char () = String.get allowed_chars @@ Random.int (String.length allowed_chars) in
  String.init (random_int 5 15) (fun _ -> char ())

let primitive () = match Random.int 10 with
  | 0 | 1 -> Null
  | 2 | 3 -> String_lit (unquoted_string ())
  | 4 -> Int_lit (Random.bits ())
  | 5 -> Int_lit (-Random.bits ())
  | 6 -> Float_lit (Random.float max_float)
  | 7 -> Float_lit (Random.float min_float)
  | 8 -> Bool_lit true
  | 9 -> Bool_lit false
  | _ -> assert false

let ocaml_list length ?(upto = length) spec =
  (* Use an array to stop stack overflows. *)
  Array.init (random_int length upto) (fun _ -> spec ()) |> Array.to_list

let array length ?upto spec () =
  Arr (ocaml_list length ?upto spec)

let obj length ?upto spec () =
  Obj (ocaml_list length ?upto @@ fun () -> unquoted_string (), spec ())

let array_or_obj length = if Random.bool () then array length else obj length

let rec randomly_balanced ?(list = array_or_obj) ?(min_branch = 1) ~max_branch
    ?(min_depth = 0) ~max_depth spec () =
  let min_depth = max 0 min_depth in
  if random_int min_depth max_depth = 0 then spec ()
  else
    let spec = randomly_balanced ~list ~min_branch ~max_branch
        ~min_depth:(min_depth - 1) ~max_depth:(max_depth - 1) spec in
    list min_branch ~upto:max_branch spec ()

let perfectly_balanced ?list ~branch ~depth =
  randomly_balanced ?list ~min_branch:branch ~max_branch:branch ~min_depth:depth ~max_depth:depth

let lock spec =
  let json = Lazy.from_fun spec in
  fun () -> Lazy.force json

let tokens spec =
  let rec to_tokens acc json =
    (* Use fold_left instead of fold_right as it's tail recursive. *)
    let in_order start f l close =
      start :: let acc = close :: acc in match l with
      | [] -> acc
      | hd::tl -> hd |> f (List.fold_left (fun acc v -> COMMA :: f acc v) acc (List.rev tl))
    in
    let pair_to_tokens acc (k, v) = STRING k :: COLON :: to_tokens acc v in
    match json with
    | Obj l -> in_order OBJ_START pair_to_tokens l OBJ_END
    | Arr l -> in_order ARRAY_START to_tokens l ARRAY_END
    | String_lit s -> STRING s :: acc
    | Int_lit n -> INT n :: acc
    | Float_lit f -> FLOAT f :: acc
    | Bool_lit b -> BOOL b :: acc
    | Null -> NULL :: acc
  in
  spec () |> to_tokens [END] |> Array.of_list

let raw_of_tokens tokens_array =
  let token_to_string = function
    | OBJ_START -> "{" | OBJ_END -> "}" | COLON -> ": "
    | ARRAY_START -> "[" | ARRAY_END -> "]" | COMMA -> ",\n"
    | STRING s -> "\"" ^ s ^ "\"" | INT n -> string_of_int n | FLOAT f -> string_of_float f
    | BOOL b -> string_of_bool b | NULL -> "null" | END -> ""
  in
  tokens_array |> Array.map token_to_string |> Array.to_list |> String.concat ""

let raw spec = tokens spec |> raw_of_tokens

open Gadt_rope

let split v_length uvw =
  let n = length uvw in
  let i = Random.int (n - v_length) in
  let j = i + v_length in
  let uv, w = uvw |> split_exn j in
  let u, v = uv |> split_exn i in
  (u, v, w)
