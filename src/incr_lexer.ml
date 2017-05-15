type 'tok t = {
  lex_at : int -> 'tok * int;
  pos : int;
  next : ('tok * int) Lazy.t;
}

type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

let make_at pos lex_at = {lex_at; pos; next = lazy (lex_at pos)}

let make lex_at = make_at 0 lex_at

let handle_errors {lex_at; pos; _} =
  (* TODO need to do something with error messages. *)
  let rec lex_until_token total_len pos = match lex_at pos with
    | Token token, len | Error_with_token (token, _), len -> token, total_len + len
    | Error_msg _, len -> lex_until_token (total_len + len) (pos + len)
  in
  make_at pos (lex_until_token 0)

let safe_array_get a i = a.(min i (Array.length a - 1))

let of_token_array a = make (fun i -> safe_array_get a i, 1)

let of_token_and_length_array a = make (fun i -> safe_array_get a i)

let of_ocamllex lex ~make_lexbuf_at =
  let lexbuf_ref = ref (make_lexbuf_at 0) in
  let last_pos_ref = ref 0 in
  make @@ fun pos ->
    let lexbuf =
      if pos <> !last_pos_ref then lexbuf_ref := make_lexbuf_at pos;
      !lexbuf_ref
    in
    (* The position given by lexbuf may differ from pos. *)
    let lexbuf_pos = Lexing.lexeme_end lexbuf in
    let lex_result = lex lexbuf in
    let length = Lexing.lexeme_end lexbuf - lexbuf_pos in
    last_pos_ref := pos + length;
    lex_result, length

let of_ocamllex_and_string lex s =
  let pos_ref = ref 0 in
  let lexing_function b n =
    let len = min n (String.length s - !pos_ref) in
    Bytes.blit_string s !pos_ref b 0 len;
    pos_ref := !pos_ref + len;
    len
  in
  let make_lexbuf_at pos = pos_ref := pos; Lexing.from_function lexing_function in
  of_ocamllex lex ~make_lexbuf_at

let pos {pos; _} = pos

let verbose = ref false

let next {lex_at; pos; next = lazy (token, len)} =
  if !verbose then Printf.printf "Lexing at position %i.\n" pos;
  token, len, make_at (pos + len) lex_at

let peek {next = lazy (token, _); _} = token

let skip n ({lex_at; pos; _} as t) = if n = 0 then t else make_at (pos + n) lex_at

let move_to pos' ({lex_at; pos; _} as t) = if pos = pos' then t else make_at pos' lex_at
