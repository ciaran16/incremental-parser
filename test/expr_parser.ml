open Expr_lexer

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

module Tag = struct
  open Tagging

  type _ t =
    | Expr : expr t
    | Expr_option : expr option t

  let tags_equal : type a b. a t -> b t -> (a, b) equal = fun a_tag b_tag ->
    match a_tag, b_tag with
    | Expr, Expr -> Equal
    | Expr_option, Expr_option -> Equal
    | _ -> Not_equal
end

module Incr_parsing = Incr_parsing.Make (Tag)
open Incr_parsing
open Combinators

let expr = fix @@ fun expr ->
  let if_ e1 e2 e_o = If (e1, e2, e_o) in
  let let_ x e1 e2 = Let (x, e1, e2) in
  let ident = satisfy (function IDENT x -> Some x | _ -> None) in
  let parse_else =
    let prefixes = function
      | ELSE ->  Prefix.custom ((fun e -> Some e) <$> expr)
      | _ ->     Prefix.unknown
    in
    pratt_parser Tag.Expr_option ~prefixes ~empty_prefix:(Prefix.return None)
  in
  let prefixes = function
    | INT n ->   Prefix.return (Int_lit n)
    | BOOL b ->  Prefix.return (Bool_lit b)
    | PLUS ->    Prefix.unary  (fun e -> e)
    | MINUS ->   Prefix.unary  (fun e -> Neg e)
    | IDENT x -> Prefix.return (Var x)
    | PAREN_L -> Prefix.custom (expr <* eat PAREN_R)
    | IF ->      Prefix.custom (if_ <$> expr <*> eat THEN *> expr <*> parse_else)
    | LET ->     Prefix.custom (let_ <$> ident <*> eat EQUALS *> expr <*> eat IN *> expr)
    | _ ->       Prefix.unknown
  in
  let infixes = function
    | FACT ->    Infix.postfix (fun e -> Fact e)
    | POW ->     Infix.right 1 (fun e1 e2 -> Pow (e1, e2))
    | TIMES ->   Infix.left  2 (fun e1 e2 -> Times (e1, e2))
    | DIVIDE ->  Infix.left  2 (fun e1 e2 -> Divide (e1, e2))
    | PLUS ->    Infix.left  3 (fun e1 e2 -> Plus (e1, e2))
    | MINUS ->   Infix.left  3 (fun e1 e2 -> Minus (e1, e2))
    | EQUALS ->  Infix.left  4 (fun e1 e2 -> Equals (e1, e2))
    | AND ->     Infix.right 5 (fun e1 e2 -> And (e1, e2))
    | OR ->      Infix.right 6 (fun e1 e2 -> Or (e1, e2))
    | _ ->       Infix.unknown
  in
  pratt_parser Tag.Expr ~prefixes ~infixes
