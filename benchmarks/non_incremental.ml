open Benchmarking

let read_data name =
  let file = "benchmarks/data/" ^ name in
  let fd = Unix.(openfile file [O_RDONLY] 0o644) in
  let length = Unix.((fstat fd).st_size) in
  let bytes = Bytes.create length in
  let rec loop pos remaining =
    let n = Unix.read fd bytes pos remaining in
    if n = 0 then () else loop (pos + n) (remaining - n)
  in
  loop 0 length; Bytes.to_string bytes

let rec equals_yojson (json : Json_parser.json) (yojson : Yojson.Basic.json) =
  let open Json_parser in
  let pair_equal (k1, json) (k2, yojson) = k1 = k2 && equals_yojson json yojson in
  let all_equal equal l1 l2 =
    try List.for_all2 equal l1 l2
    with Invalid_argument _ -> false
  in
  match json, yojson with
  | Obj json_l, `Assoc yojson_l -> all_equal pair_equal json_l yojson_l
  | Arr json_l, `List yojson_l -> all_equal equals_yojson json_l yojson_l
  | String_lit s1, `String s2 -> s1 = s2
  | Int_lit n1, `Int n2 -> n1 = n2
  | Float_lit f1, `Float f2 -> f1 = f2
  | Bool_lit b1, `Bool b2 -> b1 = b2
  | Null, `Null -> true
  | _ -> false

let () =
  Random.self_init ();
  let raw_json = Generate_json.(raw @@ perfectly_balanced primitive ~branch:100 ~depth:3) in
  let open Core_bench.Std.Bench in
  let tests = [
    Test.create ~name:"Incremental library" (fun () ->
        let make_lexbuf_at pos = assert (pos = 0); Lexing.from_string raw_json in
        let lexer = Incr_lexing.Lexer.of_ocamllex Json_lexer.lex ~make_lexbuf_at in
        Incr_parsing.Parse_tree.(create Json_parser.value ~lexer |> value)
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
  ] in
  let run_config = Run_config.create ()
      ~verbosity:`High
      ~time_quota:(Core.Std.Time.Span.of_sec 1.0)
      ~fork_each_benchmark:true
  in
  bench tests ~run_config ~display_config:(Display_config.create ~show_samples:true ())
