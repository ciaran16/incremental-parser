open Benchmarking
open Incr_lexing
open Incr_parsing

let huge_incremental () =
  (* These don't work well when run through Core_bench as it always tries to stabilise the garbage
     collector on first run, even with stabilize_gc_between_runs set to false. *)
  let raw_json = read_data "citylots.json" in
  let pos_ref = ref 0 in
  let lexing_function b n =
    let len = min n (String.length raw_json - !pos_ref) in
    Bytes.blit_string raw_json !pos_ref b 0 len;
    pos_ref := !pos_ref + len;
    len
  in
  let make_lexbuf_at pos = pos_ref := pos; Lexing.from_function lexing_function in
  let lexer = Incr_lexer.of_ocamllex Json_lexer.lex ~make_lexbuf_at in
  let pt = with_time (fun () -> Parse_tree.create Json_parser.value ~lexer) in
  let start = Parse_tree.length pt / 2 in
  let pt' = with_time @@ fun () ->
    let lexer = Incr_lexer.move_to 0 lexer in
    Parse_tree.update pt ~start ~added:1 ~removed:1 ~lexer
  in
  let pt' = with_time @@ fun () ->
    let lexer = Incr_lexer.move_to 0 lexer in
    Parse_tree.update pt' ~start ~added:10 ~removed:10 ~lexer
  in
  let pt' = with_time @@ fun () ->
    let lexer = Incr_lexer.move_to 0 lexer in
    Parse_tree.update pt' ~start ~added:1000 ~removed:1000 ~lexer
  in
  assert Parse_tree.(value pt = value pt' && length pt = length pt');
  assert (equals_yojson (Parse_tree.value pt') (Yojson.Basic.from_string raw_json))
