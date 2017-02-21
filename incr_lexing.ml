type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

type 'tok lex_f = int -> 'tok lex_result * int

module Lexer = struct
  type 'tok t = {
    lex : 'tok lex_f;
    pos : int;
    next : ('tok * int) Lazy.t;
  }

  let start_at pos lex =
    (* TODO need to do something with error messages. *)
    let rec lex_until_token pos = match lex pos with
      | Token token, len | Error_with_token (token, _), len -> token, pos + len
      | Error_msg _, len -> lex_until_token (pos + len)
    in
    {lex; pos; next = lazy (lex_until_token pos)}

  let next {lex; next; _} = let token, pos = Lazy.force next in token, lex |> start_at pos

  let peek {next; _} = Lazy.force next |> fst

  let skip n {lex; pos; _} = lex |> start_at (pos + n)

  let pos {pos; _} = pos
end
