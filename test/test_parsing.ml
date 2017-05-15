open Incr_parsing

module Non_incremental_parsing = struct
  let check_length s () =
    let lexer = Incr_lexer.of_ocamllex_and_string Expr_lexer.lex s in
    let parse_tree = Parse_tree.create Expr_parser.expr ~lexer in
    Alcotest.(check int) "same length" (String.length s) (Parse_tree.length parse_tree)

  let tests = [
    "Empty prefix length 1", `Quick, check_length "if true then 2";
    "Empty prefix length 2", `Quick, check_length "(if true then 2) + 4";
  ]
end

module Incremental_parsing = struct
  let expr_testable = Alcotest.of_pp Expr_parser.pretty_print

  let check_expr = Alcotest.check expr_testable

  let compare first second ~start ~added ~removed =
    let first_lexer = Incr_lexer.of_ocamllex_and_string Expr_lexer.lex first in
    let parse_tree = Parse_tree.create Expr_parser.expr ~lexer:first_lexer in
    let second_lexer = Incr_lexer.of_ocamllex_and_string Expr_lexer.lex second in
    let e1 = Parse_tree.create Expr_parser.expr ~lexer:second_lexer in
    Incr_lexer.verbose := true; print_reuse_info := true;
    let e2 = parse_tree |> Parse_tree.update ~start ~added ~removed ~lexer:second_lexer in
    Incr_lexer.verbose := false; print_reuse_info := false;
    check_expr "parses equal" (Parse_tree.value e1) (Parse_tree.value e2);
    Alcotest.(check int "lengths equal" (Parse_tree.length e1) (Parse_tree.length e2))

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

  let update_double_if () = compare
      "if x then if y then 1 else 2"
      "if x then 1 else 2"
      ~start:9 ~added:0 ~removed:10

  let tests = [
    "Incremental update", `Quick, update;
    "Token update", `Quick, update_token;
    "Update just before empty prefix operator", `Quick, update_empty;
    "Update at end", `Quick, update_end;
    "Update nested", `Quick, update_nested;
    "Update right associative infixes", `Quick, update_right_assoc_infixes;
    "Update left associative infixes", `Quick, update_left_assoc_infixes;
    "Update double if", `Quick, update_double_if;
  ]
end

let () =
  Alcotest.run "Parsing tests" [
    "Non-incremental parsing", Non_incremental_parsing.tests;
    "Incremental parsing", Incremental_parsing.tests;
  ]
