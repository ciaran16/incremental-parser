open Incr_lexing
module Rope = Gadt_rope.String_rope
module Iterator = Rope.Fast_iterator

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

let lex_keyword s tok iter =
  let rec check i iter =
    if i = String.length s then token tok, iter
    else
      let c, iter' = Iterator.next iter in
      if Iterator.is_at_end iter || s.[i] <> c then
        error ("Unrecognised '" ^ String.sub s 0 i ^ "'."), iter
      else check (i + 1) iter'
  in
  check 0 iter

(* Note that this does not exactly match the JSON spec (e.g. it allows things like '4.' and 04). *)
let rec lex_number cs iter =
  let c, iter' = Iterator.next iter in match c with
  | '-' | '+' | '0'..'9' | '.' | 'e' | 'E' when not (Iterator.is_at_end iter) ->
    lex_number (c::cs) iter'
  | _ ->
    let s = cs |> List.rev |> List.map (String.make 1) |> String.concat "" in
    try token (NUMBER (float_of_string s)), iter
    with _ -> error ("Incorrectly formatted number '" ^ s ^ "'."), iter

let rec lex_string ss iter =
  let tok () = STRING (ss |> List.rev |> String.concat "") in
  if Iterator.is_at_end iter then error "Unterminated string." ~token:(tok ()), iter
  else
    let c, iter = Iterator.next iter in match c with
    | '"' -> token (tok ()), iter
    | '\\' ->
      if Iterator.is_at_end iter then error "Unterminated string." ~token:(tok ()), iter
      else
        let c, iter = Iterator.next iter in
        begin match c with
          | 'u' -> lex_string ss (iter |> Iterator.skip 4) (* NOTE hex isn't handled. *)
          | _ ->
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
    | s -> lex_string (String.make 1 s::ss) iter

let rec lex iter =
  if Iterator.is_at_end iter then token END, iter
  else
    let c, iter = Iterator.next iter in match c with
    | ' ' | '\n' | '\r' | '\t' -> lex iter
    | 't' -> lex_keyword "rue" (BOOL true) iter
    | 'f' -> lex_keyword "alse" (BOOL false) iter
    | 'n' -> lex_keyword "ull" NULL iter
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
  in pratt_parser prefixes

let () =
  let rec read b =
    let s = read_line () in
    if s = "stop" then Buffer.contents b
    else begin Buffer.add_string b s; Buffer.add_char b '\n'; read b end
  in
  let s = read (Buffer.create 1024) in
  let t = Sys.time () in
  let rope = Rope.of_string s in
  let tokens = lex |> Incr_lexing.Non_incremental.lex_all rope ~end_token:END in
  value |> Non_incremental.run ~tokens |> ignore;
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
