open Incr_parsing

let expr_lexer_from_string s =
  let make_lexbuf_at pos = Lexing.from_string (String.sub s pos (String.length s - pos)) in
  Incr_lexing.Lexer.(handle_errors @@ of_ocamllex Expr_lexer.lex ~make_lexbuf_at)

module Non_incremental_parsing = struct
  let check_length s () =
    let lexer = expr_lexer_from_string s in
    let parse_tree = Incremental.make Expr_parser.expr ~lexer in
    Alcotest.(check int) "same length" (String.length s) (Incremental.length parse_tree)

  let tests = [
    "Empty prefix length 1", `Quick, check_length "if true then 2";
    "Empty prefix length 2", `Quick, check_length "(if true then 2) + 4";
    "Lexing errors length", `Quick, check_length "2 # + . 2 = \n@ 4";
    "Parsing unknown infix length", `Quick, check_length "2 + 2) = 4"
  ]
end

module Incremental_parsing = struct
  let expr_testable = Alcotest.of_pp (fun ppf _e -> Format.pp_print_string ppf "<expr>")

  let check_expr = Alcotest.check expr_testable

  let compare first second ~start ~added ~removed =
    let incr = Incremental.make Expr_parser.expr ~lexer:(expr_lexer_from_string first) in
    let lexer = expr_lexer_from_string second in
    let e1 = Incremental.make Expr_parser.expr ~lexer |> Incremental.to_ast in
    let e2 = incr |> Incremental.update ~start ~added ~removed ~lexer |> Incremental.to_ast in
    check_expr "parses equal" e1 e2

  let update () = compare
      "3 * (4 + 5 * 6!) - -3"
      "3 * (4 * 2 - 5 * 6!) - -3"
      ~start:7 ~added:5 ~removed:1

  let update_token () = compare
      "3 * 2 - -3"
      "3 * 24 - -3"
      ~start:5 ~added:1 ~removed:0

  let update_empty () = compare
      "if true then 2 + 2"
      "if true then 2 else 4 + 2"
      ~start:14 ~added:7 ~removed:0

  let update_end () = compare
      "if true then 2"
      "if true then 2 + 2"
      ~start:14 ~added:4 ~removed:0

  let update_nested () = compare
      "(((1 + 2) * 3) + 4) * 5"
      "(((-1 + 2) * 3) + 4) * 5"
      ~start:3 ~added:1 ~removed:0

  let update_right_assoc_infixes () = compare
      "1 ^ 3 ^ 4 ^ 5"
      "1 ^ 2 ^ 3 ^ 4 ^ 5"
      ~start:1 ~added:4 ~removed:0

  let update_left_assoc_infixes () = compare
      "1 + 2 + 3"
      "1 - 4 + 2 + 3"
      ~start:1 ~added:4 ~removed:0

  let tests = [
    "Incremental update", `Quick, update;
    "Token update", `Quick, update_token;
    "Update just before empty prefix operator", `Quick, update_empty;
    "Update at end", `Quick, update_end;
    "Update nested", `Quick, update_nested;
    "Update right associative infixes", `Quick, update_right_assoc_infixes;
    "Update left associative infixes", `Quick, update_left_assoc_infixes;
  ]
end

let () =
  Alcotest.run "Parsing tests" [
    "Non-incremental parsing", Non_incremental_parsing.tests;
    "Incremental parsing", Incremental_parsing.tests;
  ]
