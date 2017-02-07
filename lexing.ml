type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)
