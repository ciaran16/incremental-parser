open Imperative_lang_lexer
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
  | Bool_lit of bool
  | Equals of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Var of string

type statement =
  | No_statement
  | Branch of statement * statement
  | Assign of string * expr
  | If of expr * statement * statement
  | While of expr * statement
  | For of statement * statement * statement * statement

let expr = fix @@ fun expr ->
  let prefixes = function
    | INT n ->   Prefix.return (Int_lit n)
    | BOOL b ->  Prefix.return (Bool_lit b)
    | PLUS ->    Prefix.unary  (fun e -> e)
    | MINUS ->   Prefix.unary  (fun e -> Neg e)
    | IDENT x -> Prefix.return (Var x)
    | L_PAREN -> Prefix.custom (expr <* eat R_PAREN)
    | _ ->       Prefix.unknown
  in
  let infixes = function
    | FACT ->    Infix.postfix (fun e -> Fact e)
    | POW ->     Infix.right 1 (fun e1 e2 -> Pow (e1, e2))
    | TIMES ->   Infix.both  2 (fun e1 e2 -> Times (e1, e2))
    | DIVIDE ->  Infix.left  2 (fun e1 e2 -> Divide (e1, e2))
    | PLUS ->    Infix.both  3 (fun e1 e2 -> Plus (e1, e2))
    | MINUS ->   Infix.left  3 (fun e1 e2 -> Minus (e1, e2))
    | EQUALS ->  Infix.both  4 (fun e1 e2 -> Equals (e1, e2))
    | AND ->     Infix.both  5 (fun e1 e2 -> And (e1, e2))
    | OR ->      Infix.both  6 (fun e1 e2 -> Or (e1, e2))
    | _ ->       Infix.unknown
  in
  pratt_parser ~prefixes ~infixes ()

let statement = fix @@ fun stmt ->
  let paren p = eat L_PAREN *> p <* eat R_PAREN in
  let parse_if =
    let prefixes = function
      | ELSE -> Prefix.custom stmt
      | _ ->    Prefix.unknown
    in
    let empty_prefix = Prefix.return No_statement in
    let parse_else = pratt_parser ~prefixes ~empty_prefix () in
    (fun e s1 s2 -> If (e, s1, s2)) <$> paren expr <*> stmt <*> parse_else
  in
  let parse_while = (fun e s -> While (e, s)) <$> paren expr <*> stmt in
  let parse_for =
    (fun init incr stop body -> For (init, incr, stop, body)) <$>
    eat L_PAREN *> stmt <*> eat SEMICOLON *> stmt <*> eat SEMICOLON *> stmt <* eat R_PAREN <*> stmt
  in
  let stmts =
    let infixes = function
      | SEMICOLON -> Infix.both 1 (fun s1 s2 -> Branch (s1, s2))
      | _ -> Infix.unknown
    in
    let empty_prefix = Prefix.custom stmt in
    pratt_parser ~infixes ~empty_prefix ()
  in
  let prefixes = function
    | L_BRACE -> Prefix.custom (stmts <* eat R_BRACE)
    | IDENT x -> Prefix.custom ((fun e -> Assign (x, e)) <$> eat ASSIGN *> expr)
    | IF ->      Prefix.custom parse_if
    | WHILE ->   Prefix.custom parse_while
    | FOR ->     Prefix.custom parse_for
    | _ ->       Prefix.unknown
  in
  let empty_prefix = Prefix.return No_statement in
  pratt_parser ~prefixes ~empty_prefix ()
