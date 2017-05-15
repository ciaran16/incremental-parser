open Benchmarking
open Incr_parsing

type timing = {
  length : int;
  incr : float;
  yojson : float;
  angstrom : float;
}

let rep = 5

let test_json i raw_json =
  let length = String.length raw_json in
  Printf.printf "%i) %f MB\n%!" i @@ float_of_int length /. 1000000.0;
  let yojson = measure_time rep @@ fun () -> Yojson.Basic.from_string raw_json in
  let incr = measure_time rep @@ fun () ->
    let lexer = Incr_lexer.of_ocamllex_and_string Json_lexer.lex raw_json in
    Parse_tree.create Json_parser.value ~lexer |> Parse_tree.value
  in
  let angstrom = measure_time rep @@ fun () ->
    match Angstrom.parse_only Angstrom_json.json (`String raw_json) with
    | Ok _ -> ()
    | Error err -> failwith err
  in
  {length; incr; yojson; angstrom}

let () =
  Random.init 319744957;
  let raw_jsons = Array.init 20 @@ fun i ->
    let open Generate.Json in
    raw @@ array_or_obj (5 * (i + 1)) @@
    randomly_balanced ~min_branch:5 ~max_branch:10 ~max_depth:5 primitive
  in
  let rows = Array.mapi test_json raw_jsons in
  Array.sort (fun a b -> compare a.length b.length) rows;
  rows |> write_results "initialjson.dat"
    ~headings:["length"; "incr"; "yojson"; "angstrom"]
    ~to_string:(fun {length; incr; yojson; angstrom} ->
        Printf.sprintf "%i %f %f %f" length incr yojson angstrom
      );
  let ratios = Array.map (fun {incr; yojson; _} -> incr /. yojson) rows in
  let sum = Array.fold_right (+.) ratios 0.0 in
  let avg = sum /. 20.0 in
  Printf.printf "Yojson was %f times quicker.\n%!" avg;
  (* Check parser gives the same result as Yojson. *)
  let all_equal = raw_jsons |> Array.for_all (fun raw_json ->
      let lexer = Incr_lexer.of_ocamllex_and_string Json_lexer.lex raw_json in
      let json = Parse_tree.create Json_parser.value ~lexer |> Parse_tree.value in
      let yojson = Yojson.Basic.from_string raw_json in
      equals_yojson json yojson
    ) in
  assert all_equal; print_endline "Results equal."
