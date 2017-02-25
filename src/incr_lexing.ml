type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

module Lexer = struct
  type 'tok t = {
    lex_at : 'tok lex_at_function;
    pos : int;
    next : ('tok * int) Lazy.t;
  }

  and 'tok lex_at_function = (int -> 'tok lex_result * int)

  let make_at pos lex_at =
    (* TODO need to do something with error messages. *)
    let rec lex_until_token pos = match lex_at pos with
      | Token token, len | Error_with_token (token, _), len -> token, pos + len
      | Error_msg _, len -> lex_until_token (pos + len)
    in
    {lex_at; pos; next = lazy (lex_until_token pos)}

  let make lex_at = make_at 0 lex_at

  let of_token_list l =
    let a = Array.of_list l in
    let len = Array.length a in
    make @@ fun i -> Token a.(max i (len - 1)), 1

  let of_ocamllex lex ~make_lexbuf_at =
    let lexbuf_ref = ref (make_lexbuf_at 0) in
    let last_pos_ref = ref 0 in
    let lex_at pos =
      let lexbuf =
        if pos <> !last_pos_ref then lexbuf_ref := make_lexbuf_at pos else print_endline "reusing lexbuf";
        !lexbuf_ref
      in
      (* The position given by lexbuf may differ from pos. *)
      let lexbuf_pos = Lexing.lexeme_end lexbuf in
      let lex_result = lex lexbuf in
      let length = Lexing.lexeme_end lexbuf - lexbuf_pos in
      last_pos_ref := pos + length;
      lex_result, length
    in
    make lex_at

  let pos {pos; _} = pos

  let next {lex_at; pos; next = lazy (token, pos')} =
    token, pos' - pos, make_at pos' lex_at

  let peek {next = lazy (token, _); _} = token

  let skip n ({lex_at; pos; _} as t) = if n = 0 then t else make_at (pos + n) lex_at

  let move_to pos' ({lex_at; pos; _} as t) = if pos = pos' then t else make_at pos' lex_at
end
