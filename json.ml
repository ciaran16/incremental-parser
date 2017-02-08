open Incr_lexing
module Iterator = Gadt_rope.Iterator

type token =
  | END
  | OBJ_START | OBJ_END
  | ARRAY_START | ARRAY_END
  | COLON
  | COMMA
  | BOOL of bool
  | NULL
  | NUMBER of float
  | STRING of string

let lex_keyword s tok ~iter =
  let rec check i iter =
    if i = String.length s then token tok, iter
    else match Iterator.next iter with
      | Some next, iter when s.[i] = next -> check (i + 1) iter
      | _ -> error ("Unrecognised '" ^ String.sub s 0 i ^ "'."), iter
  in
  check 0 iter

(* Note that this does not exactly match the JSON spec (e.g. it allows things like '4.' and 04). *)
let rec lex_number cs iter =
  let next, iter' = Iterator.next iter in
  match next with
  | Some ('-' | '+' | '0'..'9' | '.' | 'e' | 'E' as c) -> lex_number (c::cs) iter'
  | _ ->
    let s = cs |> List.rev |> List.map (String.make 1) |> String.concat "" in
    try token (NUMBER (float_of_string s)), iter
    with _ -> error ("Incorrectly formatted number '" ^ s ^ "'."), iter

let rec lex_string ss iter =
  let next, iter = Iterator.next iter in
  let tok () = STRING (ss |> List.rev |> String.concat "") in
  match next with
  | None -> error "Unterminated string." ~token:(tok ()), iter
  | Some '"' -> token (tok ()), iter
  | Some '\\' ->
    let next, iter = Iterator.next iter in
    begin match next with
      | None -> error "Unterminated string." ~token:(tok ()), iter
      | Some 'u' -> lex_string ss (iter |> Iterator.drop 4) (* NOTE hex isn't handled. *)
      | Some c ->
        let s = match c with
          | 'b' -> "\x08"
          | 'f' -> "\x0c"
          | 'n' -> "\n"
          | 'r' -> "\r"
          | 't' -> "\t"
          | _ -> String.make 1 c
        in
        lex_string (s::ss) iter
    end
  | Some s -> lex_string (String.make 1 s::ss) iter

let rec lex iter =
  let next, iter = Iterator.next iter in
  match next with
  | None -> token END, iter
  | Some c ->
    match c with
    | ' ' | '\n' | '\r' | '\t' -> lex iter
    | 't' -> lex_keyword "rue" (BOOL true) ~iter
    | 'f' -> lex_keyword "alse" (BOOL false) ~iter
    | 'n' -> lex_keyword "ull" NULL ~iter
    | '0'..'9' | '-' -> lex_number [c] iter
    | '"' -> lex_string [] iter
    | _ ->
      let result = match c with
        | '{' -> token OBJ_START
        | '}' -> token OBJ_END
        | '[' -> token ARRAY_START
        | ']' -> token ARRAY_END
        | ':' -> token COLON
        | ',' -> token COMMA
        | _ -> error ("Unrecognised '" ^ String.make 1 c ^ "'.")
      in
      result, iter

let json_rope () =
  let rec read b =
    let s = read_line () in
    if s = "stop" then Buffer.contents b
    else begin Buffer.add_string b s; Buffer.add_char b '\n'; read b end
  in
  read (Buffer.create 1024) |> Gadt_rope.of_string

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
    | OBJ_START ->
      Prefix.list pair (fun l r -> Obj_node (l, r))
        ~sep:COMMA ~stop:OBJ_END ~wrap:(fun o -> Obj o)
    | ARRAY_START ->
      Prefix.list wrap_value (fun l r -> Arr_node (l, r))
        ~sep:COMMA ~stop:ARRAY_END ~wrap:(fun a -> Arr a)
    | BOOL b ->      Prefix.return (Bool b)
    | NULL ->        Prefix.return Null
    | STRING s ->    Prefix.return (Str s)
    | NUMBER n ->    Prefix.return (Number n)
    | _ -> failwith "Unknown prefix."
in pratt_parser prefixes

let parse json =
  let tokens = lex |> Incr_lexing.Non_incremental.lex_all json ~end_token:END in
  value |> Non_incremental.run ~tokens ~end_token:END
