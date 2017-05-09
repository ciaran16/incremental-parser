type seconds = float

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

let time_of f =
  let time1 = Sys.time () in
  let v = f () in
  let time2 = Sys.time () in
  time2 -. time1, v

let time_of_unix f =
  let time1 = Unix.gettimeofday () in
  let v = f () in
  let time2 = Unix.gettimeofday () in
  time2 -. time1, v

let print_time time = Printf.printf "Time: %fs" time; print_newline ()

let print_time_of f = time_of f |> fst |> print_time

let print_time_of_unix f = time_of_unix f |> fst |> print_time
