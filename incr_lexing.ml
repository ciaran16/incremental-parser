module Iterator = Gadt_rope.Iterator

type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

type ('char, 'tok) lex_f = 'char Iterator.t -> 'tok lex_result * 'char Iterator.t

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

(* TODO need to do something with error messages. *)

module Non_incremental = struct
  let lex_all rope ~end_token lexer =
    let rec loop iter =
      match lexer iter with
      | Error_msg _, iter -> loop iter
      | Error_with_token (tok, _), iter -> tok :: loop iter
      | Token tok, iter -> tok :: loop iter
    in
    rope |> Gadt_rope.iterator |> loop |> Gadt_rope.of_list
end

module Incremental = struct
  type ('char, 'tok) t = {
    rope : 'char Gadt_rope.t;
    lexer : ('char, 'tok) lex_f;
    end_token : 'tok;
    tokens : 'tok Gadt_rope.t;
  }

  let make rope ~end_token lexer =
    let tokens = lexer |> Non_incremental.lex_all rope ~end_token in
    {rope; lexer; end_token; tokens}
end
