open Core_bench.Std.Bench

let run_tests name init_tests =
  let run_config = Run_config.create ()
      ~verbosity:`High
      ~time_quota:(Core.Span.of_sec 1.0)
  in
  let display_config = Display_config.create ()
      ~show_samples:true
      ~show_percentage:true
      ~show_speedup:true
  in
  print_string "Running tests: "; print_endline name;
  let tests = init_tests () in
  bench tests ~run_config ~display_config

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
  let time1 = Unix.gettimeofday () in
  let v = f () in
  let time2 = Unix.gettimeofday () in
  Printf.printf "Time taken: %fs" (time2 -. time1); print_newline (); v

let rec equals_yojson (json : Json_parser.json) (yojson : Yojson.Basic.json) =
  let open Json_parser in
  let pair_equal (k1, json) (k2, yojson) = k1 = k2 && equals_yojson json yojson in
  let all_equal equal l1 l2 =
    try List.for_all2 equal l1 l2
    with Invalid_argument _ -> false
  in
  let rec inorder acc = function
    | `Leaf v -> v::acc
    | `Branch (l, r) ->
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
