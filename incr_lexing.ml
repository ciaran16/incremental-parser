module Rope = Gadt_rope.String_rope
module Iterator = Rope.Fast_iterator
module F_array = Gadt_rope.Functional_array

type 'tok lex_result =
  | Token of 'tok
  | Error_msg of string
  | Error_with_token of 'tok * string

type 'tok lex_f = char Iterator.t -> 'tok lex_result * char Iterator.t

let token tok = Token tok

let error ?token msg =
  match token with
  | None -> Error_msg msg
  | Some token -> Error_with_token (token, msg)

(* TODO need to do something with error messages. *)

module Non_incremental = struct
  (* TODO this needs doing properly. *)
  let lex_all rope ~end_token lexer =
    let rec loop iter =
      if Iterator.is_at_end iter then [end_token]
      else
        match lexer iter with
        | Error_msg _, iter -> loop iter
        | Error_with_token (tok, _), iter | Token tok, iter -> tok :: loop iter
    in
    match rope |> Iterator.start_at 0 with
    | None -> F_array.of_array [||]
    | Some iter -> loop iter |> Array.of_list |> F_array.of_array
end

module Incremental = struct
  type 'tok t = {
    rope : char Rope.t;
    lexer : 'tok lex_f;
    tokens : 'tok F_array.t;
    end_token : 'tok;
  }

  let make rope ~end_token lexer =
    let tokens = lexer |> Non_incremental.lex_all rope ~end_token in
    {rope; lexer; tokens; end_token}
end
