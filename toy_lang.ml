open Incr_parsing
open Combinators

type token =
  | END
  | INT_LIT of int
  | PLUS | MINUS | TIMES | POW | FACT
  | BOOL of bool
  | EQUAL | AND | OR
  | PAREN_L | PAREN_R
  | IF | THEN | ELSE
  | LET | IN
  | IDENT of string

type expr =
  | Int_lit of int
  | Neg of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Pow of expr * expr
  | Fact of expr
  | Bool_lit of bool
  | Equal of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Var of string

let make_if e1 e2 e3 = If (e1, e2, e3)

let make_let x e1 e2 = Let (x, e1, e2)

let expr = fix @@ fun expr ->
  let prefixes = function
    | INT_LIT n -> Prefix.return (Int_lit n)
    | BOOL b ->    Prefix.return (Bool_lit b)
    | PLUS ->      Prefix.unary  (fun e -> e)
    | MINUS ->     Prefix.unary  (fun e -> Neg e)
    | IDENT x ->   Prefix.return (Var x)
    | IF ->
      Prefix.custom (make_if <$> expr <*> eat THEN *> expr <*> eat ELSE *> expr)
    | LET ->
      let ident = satisfy (function IDENT x -> Some x | _ -> None) in
      Prefix.custom (make_let <$> ident <*> eat EQUAL *> expr <*> eat IN *> expr)
    | _ -> failwith "Unknown prefix."
  in
  let infixes = function
    | FACT ->   Infix.postfix (fun e -> Fact e)
    | POW ->    Infix.right 1 (fun e1 e2 -> Pow (e1, e2))
    | TIMES ->  Infix.left  2 (fun e1 e2 -> Times (e1, e2))
    | PLUS ->   Infix.left  3 (fun e1 e2 -> Plus (e1, e2))
    | MINUS ->  Infix.left  3 (fun e1 e2 -> Minus (e1, e2))
    | EQUAL ->  Infix.left  4 (fun e1 e2 -> Equal (e1, e2))
    | AND ->    Infix.right 5 (fun e1 e2 -> And (e1, e2))
    | OR ->     Infix.right 6 (fun e1 e2 -> Or (e1, e2))
    | _ ->      Infix.unknown
  in
  pratt_parser ~prefixes ~infixes

let f () =
  let l = [LET; IDENT "y"; EQUAL; INT_LIT 4; IN; IDENT "x"; EQUAL; IDENT "y"] in
  let tokens = Gadt_rope.of_list l in
  let v1, incr = Incremental.make expr ~tokens ~end_token:END in
  let start = 3 in
  let tokens = tokens |> Gadt_rope.delete_exn start |> Gadt_rope.insert_exn start (INT_LIT 5) in
  let v2 = incr |> Incremental.update ~start ~added:2 ~removed:0 ~tokens |> fst in
  v1, v2

(* TODO The (2 ^ 2) doesn't get reused here. *)
let g () =
  let l = [INT_LIT 1; PLUS; INT_LIT 2; POW; INT_LIT 2; TIMES; INT_LIT 3; PLUS; INT_LIT 4] in
  let tokens = Gadt_rope.of_list l in
  let v1, incr = Incremental.make expr ~tokens ~end_token:END in
  let start = 1 in
  let tokens = tokens |> Gadt_rope.delete_exn start |> Gadt_rope.insert_exn start MINUS |> Gadt_rope.insert_exn (start + 1) (INT_LIT 0) |> Gadt_rope.insert_exn (start + 2) TIMES in
  let v2, _incr = incr |> Incremental.update ~start ~added:3 ~removed:1 ~tokens in
  v1, v2