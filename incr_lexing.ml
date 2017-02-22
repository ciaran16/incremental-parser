type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

type 'tok lexer = int -> 'tok lex_result * int

let of_list l =
  let a = Array.of_list l in
  let len = Array.length a in
  let end_token = Token a.(len - 1) in
  fun i -> (if i >= len then end_token else Token a.(i)), 1

module Lexer = struct
  type 'tok t = {
    lex : 'tok lexer;
    pos : int;
    next : ('tok * int) Lazy.t;
  }

  let make_at pos lex =
    (* TODO need to do something with error messages. *)
    let rec lex_until_token pos = match lex pos with
      | Token token, len | Error_with_token (token, _), len -> token, pos + len
      | Error_msg _, len -> lex_until_token (pos + len)
    in
    {lex; pos; next = lazy (lex_until_token pos)}

  let make lex = make_at 0 lex

  let pos {pos; _} = pos

  let next {lex; pos; next = lazy (token, pos')} =
    token, pos' - pos, make_at pos' lex

  let peek {next = lazy (token, _); _} = token

  let skip n ({lex; pos; _} as t) = if n = 0 then t else make_at (pos + n) lex

  let move_to pos' ({lex; pos; _} as t) = if pos = pos' then t else make_at pos' lex
end
