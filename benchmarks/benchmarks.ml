open Incr_lexing
open Incr_parsing
open Core_bench.Std.Bench

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

let with_time f =
  let time = Unix.gettimeofday () in
  let v = f () in
  Printf.printf "Time taken: %fs" (Unix.gettimeofday () -. time); print_newline (); v

let rec equals_yojson (json : Json_parser.json) (yojson : Yojson.Basic.json) =
  let open Json_parser in
  let pair_equal (k1, json) (k2, yojson) = k1 = k2 && equals_yojson json yojson in
  let all_equal equal l1 l2 =
    try List.for_all2 equal l1 l2
    with Invalid_argument _ -> false
  in
  let rec inorder acc = function
    | Combinators.Leaf v -> v::acc
    | Combinators.Branch (l, r) ->
      inorder (inorder acc r) l
  in
  let to_list = function
    | None -> []
    | Some tree -> inorder [] tree
  in
  match json, yojson with
  | Obj tree_o, `Assoc yojson_l -> all_equal pair_equal (to_list tree_o) yojson_l
  | Arr tree_o, `List yojson_l -> all_equal equals_yojson (to_list tree_o) yojson_l
  | String_lit s1, `String s2 -> s1 = s2
  | Int_lit n1, `Int n2 -> n1 = n2
  | Float_lit f1, `Float f2 -> f1 = f2
  | Bool_lit b1, `Bool b2 -> b1 = b2
  | Null, `Null -> true
  | _ -> false

let create_group ~(name : string) (init_tests : unit -> Test.t list) = name, init_tests

let non_incremental = create_group ~name:"Non incremental" @@ fun () ->
  let raw_json = Generate_json.(raw @@ perfectly_balanced primitive ~branch:100 ~depth:3) in
  [
    Test.create ~name:"Incremental library" (fun () ->
        let make_lexbuf_at pos = assert (pos = 0); Lexing.from_string raw_json in
        let lexer = Incr_lexer.of_ocamllex Json_lexer.lex ~make_lexbuf_at in
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
  ]

let huge_incremental () =
  (* These don't work well when run through Core_bench as it always tries to stabilise the garbage
     collector on first run, even with stabilize_gc_between_runs set to false. *)
  let raw_json = read_data "citylots.json" in
  print_int (String.length raw_json);
  let pos_ref = ref 0 in
  let lexing_function b n =
    let len = min n (String.length raw_json - !pos_ref) in
    Bytes.blit_string raw_json !pos_ref b 0 len;
    pos_ref := !pos_ref + len;
    len
  in
  let make_lexbuf_at pos = pos_ref := pos; Lexing.from_function lexing_function in
  let lexer = Incr_lexer.of_ocamllex Json_lexer.lex ~make_lexbuf_at in
  let pt = Parse_tree.create Json_parser.value ~lexer in
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
  assert Parse_tree.(value pt = value pt' && length pt = length pt')

let run_config = Run_config.create ()
    ~verbosity:`High
    ~time_quota:(Core.Span.of_sec 1.0)

let display_config = Display_config.create ()
    ~show_samples:true
    ~show_percentage:true
    ~show_speedup:true

let () =
  Random.init 0;
  [non_incremental] |>
  List.iter (function name, init_tests ->
      let tests = init_tests () in
      print_endline name;
      bench tests ~run_config ~display_config
    );
  print_endline "Huge Incremental";
  huge_incremental ()
