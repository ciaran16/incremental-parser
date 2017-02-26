open Expr_parser.Incr_parsing

let expr_lexer_from_string s =
  let make_lexbuf_at pos = Lexing.from_string (String.sub s pos (String.length s - pos)) in
  Incr_lexing.Lexer.of_ocamllex Expr_lexer.lex ~make_lexbuf_at

module Non_incremental_parsing = struct
  let check_length s () =
    let lexer = expr_lexer_from_string s in
    let parse_tree = Non_incremental.run Expr_parser.expr ~lexer in
    Alcotest.(check int) "same length" (String.length s) (Parse_tree.length parse_tree)

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

  let update_empty () =
    let lexer = expr_lexer_from_string "if true then 2" in
    let incr = Incremental.make Expr_parser.expr ~lexer in
    let lexer = expr_lexer_from_string "if true then 2 else 4" in
    let incr = incr |> Incremental.update ~start:14 ~added:7 ~removed:0 ~lexer in
    let e1 = Non_incremental.run Expr_parser.expr ~lexer |> Parse_tree.to_ast in
    let e2 = Incremental.parse_tree incr |> Parse_tree.to_ast in
    check_expr "Incremental and non-incremental parses equal" e1 e2

  let tests = [
    "Update just before empty prefix operator", `Quick, update_empty;
  ]
end

let () =
  Alcotest.run "Parsing tests" [
    "Non-incremental parsing", Non_incremental_parsing.tests;
    "Incremental parsing", Incremental_parsing.tests;
  ]
