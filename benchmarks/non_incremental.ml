open Benchmarking
open Core_bench.Std.Bench
open Incr_parsing

let () = run_tests "Non-incremental / initial parse" @@ fun () ->
  Random.init 0;
  let raw_json = Generate_json.(raw @@ randomly_balanced primitive ~max_branch:10 ~max_depth:5) in
  let lexer = Incr_lexer.of_ocamllex_and_string Json_lexer.lex raw_json in
  let json = Parse_tree.(create Json_parser.value ~lexer |> value) in
  assert (equals_yojson json @@ Yojson.Basic.from_string raw_json);
  [
    Test.create ~name:"Incremental library" (fun () ->
        let make_lexbuf_at pos = assert (pos = 0); Lexing.from_string raw_json in
        let lexer = Incr_lexer.of_ocamllex Json_lexer.lex ~make_lexbuf_at in
        Parse_tree.(create Json_parser.value ~lexer |> value)
      );
    Test.create ~name:"Yojson" (fun () ->
        Yojson.Basic.from_string raw_json
      );
    Test.create ~name:"Recursive descent" (fun () ->
        Json_rd_parser.parse (Lexing.from_string raw_json)
      );
    Test.create ~name:"Angstrom" (fun () ->
        match Angstrom.parse_only Angstrom_json.json (`String raw_json) with
        | Ok _ -> ()
        | Error err -> failwith err
      );
  ]
