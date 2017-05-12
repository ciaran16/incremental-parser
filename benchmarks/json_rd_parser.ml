open Json_lexer

type json =
  | Obj of (string * json) list
  | Arr of json list
  | String_lit of string
  | Int_lit of int
  | Float_lit of float
  | Bool_lit of bool
  | Null

let parse lexbuf =
  let next_ref = ref (lex lexbuf) in
  let next () = let token = !next_ref in next_ref := lex lexbuf; token in
  let peek () = !next_ref in
  let eat token = if next () <> token then failwith "Eat error." in
  let list_of p ~sep ~close =
    let rec comma_list acc =
      if peek () = close then begin eat close; List.rev acc end
      else begin eat sep; comma_list (p ()::acc) end
    in
    if peek () = close then begin eat close; [] end
    else let x = p () in x :: comma_list []
  in
  let rec value () =
    let key () = match next () with STRING s -> s | _ -> failwith "Key error." in
    let pair () = let k = key () in eat COLON; let v = value () in (k, v) in
    let pair_list () = list_of pair ~sep:COMMA ~close:OBJ_END in
    let value_list () = list_of value ~sep:COMMA ~close:ARRAY_END in
    match next () with
    | OBJ_START ->   Obj (pair_list ())
    | ARRAY_START -> Arr (value_list ())
    | STRING s ->    String_lit s
    | INT n ->       Int_lit n
    | FLOAT f ->     Float_lit f
    | BOOL b ->      Bool_lit b
    | NULL ->        Null
    | _ -> failwith "Unknown."
  in
  value ()
