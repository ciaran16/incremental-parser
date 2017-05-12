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
  | Bool_lit of bool
  | Equals of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr option
  | Let of string * expr * expr
  | Var of string

let expr = fix @@ fun expr ->
  let parse_if =
    let prefixes = function
      | ELSE -> Prefix.custom ((fun e -> Some e) <$> expr)
      | _ ->    Prefix.unknown
    in
    let parse_else = pratt_parser ~prefixes ~empty_prefix:(Prefix.return None) () in
    (fun e1 e2 e_o -> If (e1, e2, e_o)) <$> expr <*> eat THEN *> expr <*> parse_else
  in
  let parse_let =
    let ident = satisfy (function IDENT x -> Some x | _ -> None) in
    (fun x e1 e2 -> Let (x, e1, e2)) <$> ident <*> eat EQUALS *> expr <*> eat IN *> expr
  in
  let prefixes = function
    | INT n ->   Prefix.return (Int_lit n)
    | BOOL b ->  Prefix.return (Bool_lit b)
    | PLUS ->    Prefix.unary  (fun e -> e)
    | MINUS ->   Prefix.unary  (fun e -> Neg e)
    | IDENT x -> Prefix.return (Var x)
    | PAREN_L -> Prefix.custom (expr <* eat PAREN_R)
    | IF ->      Prefix.custom parse_if
    | LET ->     Prefix.custom parse_let
    | _ ->       Prefix.unknown
  in
  let infixes = function
    | FACT ->    Infix.postfix (fun e -> Fact e)
    | POW ->     Infix.right 1 (fun e1 e2 -> Pow (e1, e2))
    | TIMES ->   Infix.both  2 (fun e1 e2 -> Times (e1, e2))
    | DIVIDE ->  Infix.left  2 (fun e1 e2 -> Divide (e1, e2))
    | PLUS ->    Infix.both  3 (fun e1 e2 -> Plus (e1, e2))
    | MINUS ->   Infix.left  3 (fun e1 e2 -> Minus (e1, e2))
    | EQUALS ->  Infix.left  4 (fun e1 e2 -> Equals (e1, e2))
    | AND ->     Infix.right 5 (fun e1 e2 -> And (e1, e2))
    | OR ->      Infix.right 6 (fun e1 e2 -> Or (e1, e2))
    | _ ->       Infix.unknown
  in
  pratt_parser ~prefixes ~infixes ()

let rec pretty_print ppf =
  let p = Format.pp_print_string ppf in
  let pp_prefix s e = p s; pretty_print ppf e in
  let pp_infix e1 s e2 =
    p "("; pretty_print ppf e1; p " "; p s; p " "; pretty_print ppf e2; p ")"
  in
  let pp_postfix e s = pretty_print ppf e; p s in
  function
  | Int_lit n -> Format.pp_print_int ppf n
  | Neg e -> pp_prefix "-" e
  | Plus (e1, e2) -> pp_infix e1 "+" e2
  | Minus (e1, e2) -> pp_infix e1 "-" e2
  | Times (e1, e2) -> pp_infix e1 "*" e2
  | Divide (e1, e2) -> pp_infix e1 "/" e2
  | Pow (e1, e2) -> pp_infix e1 "^" e2
  | Fact e -> pp_postfix e "!"
  | Bool_lit b -> Format.pp_print_bool ppf b
  | Equals (e1, e2) -> pp_infix e1 "=" e2
  | And (e1, e2) -> pp_infix e1 "&&" e2
  | Or (e1, e2) -> pp_infix e1 "||" e2
  | If (e1, e2, e_o) ->
    pp_prefix "if " e1; pp_prefix " then " e2;
    begin match e_o with
      | None -> ()
      | Some e3 -> pp_prefix " else " e3
    end
  | Let (x, e1, e2) -> p "let "; p x; pp_prefix " = " e1; pp_prefix " in " e2
  | Var x -> p x
