## Functional Incremental Parsing

Typical parsers take time proportional to the length of the input in the best case, as they must look at each token. Even small changes require the whole input to be parsed again to gain the up-to-date parse tree.

Incremental parsers reuse as much of the previous parse tree as possible to build the new one. This allows parsing to be performed in sub-linear time to keep the parse tree up-to-date as changes are made.

### Examples

An incremental JSON parser.

```ocaml
open Json_lexer
open Incr_parsing
open Combinators

type json =
  | Obj of (string * json) tree option
  | Arr of json tree option
  | String_lit of string
  | Int_lit of int
  | Float_lit of float
  | Bool_lit of bool
  | Null

let value = fix @@ fun value ->
  let key = satisfy (function STRING s -> Some s | _ -> None) ~on_error:"" in
  let pair = (fun k v -> (k, v)) <$> key <*> eat COLON *> value in
  let pairs = tree_of pair ~sep:COMMA ~close:OBJ_END in
  let values = tree_of value ~sep:COMMA ~close:ARRAY_END in
  let prefixes = function
    | OBJ_START ->   Prefix.custom (pairs >>| fun l -> Obj l)
    | ARRAY_START -> Prefix.custom (values >>| fun l -> Arr l)
    | STRING s ->    Prefix.return (String_lit s)
    | INT n ->       Prefix.return (Int_lit n)
    | FLOAT f ->     Prefix.return (Float_lit f)
    | BOOL b ->      Prefix.return (Bool_lit b)
    | NULL ->        Prefix.return Null
    | _ ->           Prefix.unknown
  in
  pratt_parser ~prefixes ()
```

An incremental parser for arithmetic expressions.

```ocaml
open Expr_lexer
open Incr_parsing
open Combinators

type expr =
  | Int_lit of int
  | Neg of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Pow of expr * expr
  | Fact of expr

let expr = fix @@ fun expr ->
  let prefixes = function
    | INT n ->   Prefix.return (Int_lit n)
    | BOOL b ->  Prefix.return (Bool_lit b)
    | PLUS ->    Prefix.unary  (fun e -> e)
    | MINUS ->   Prefix.unary  (fun e -> Neg e)
    | PAREN_L -> Prefix.custom (expr <* eat PAREN_R)
    | _ ->       Prefix.unknown
  in
  let infixes = function
    | FACT ->    Infix.postfix (fun e -> Fact e)
    | POW ->     Infix.right 1 (fun e1 e2 -> Pow (e1, e2))
    | TIMES ->   Infix.both  2 (fun e1 e2 -> Times (e1, e2))
    | DIVIDE ->  Infix.left  2 (fun e1 e2 -> Divide (e1, e2))
    | PLUS ->    Infix.both  3 (fun e1 e2 -> Plus (e1, e2))
    | MINUS ->   Infix.left  3 (fun e1 e2 -> Minus (e1, e2))
    | _ ->       Infix.unknown
  in
  pratt_parser ~prefixes ~infixes ()
```

### Benchmarks

So far only JSON has been properly benchmarked.

Running the initial parse (when starting the parser) is roughly 5 times slower than Yojson, an optimised, non-incremental JSON parser written in OCaml.

However, incremental parses for small changes can be extremely fast, with parsing small changes taking only a few milliseconds for files that are tens of megabytes in size. Yojson must parse the whole file again, and this can take several seconds for files this large.
