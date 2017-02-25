module Tags = struct
  type (_, _) eq =
    | Equal : ('a, 'a) eq
    | Not_equal : ('a, 'b) eq

  module Lift (T : sig type 'a t end) = struct
    let f : type a b. (a, b) eq -> (a T.t, b T.t) eq = function
      | Equal -> Equal
      | Not_equal -> Not_equal
  end
end

module Lexer = Incr_lexing.Lexer

type ('tok, _) parser =
  | Satisfy : ('tok -> 'a option) -> ('tok, 'a) parser
  | Pratt : ('tok, 'a) lookups -> ('tok, 'a) parser
  | Lift : ('a -> 'b) * ('tok, 'a) parser -> ('tok, 'b) parser
  | App : ('tok, 'a -> 'b) parser * ('tok, 'a) parser -> ('tok, 'b) parser
  | Fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser

and ('tok, _) parse_node =
  | Satisfy_node : {
      value : 'a;
      len : int;
    } -> ('tok, 'a) parse_node
  | App_node : {
      p : ('tok, 'a -> 'b) parser;
      q : ('tok, 'a) parser;
      left : ('tok, 'a -> 'b) parse_node;
      right : ('tok, 'a) parse_node;
      value : 'b;
      len : int;
    } -> ('tok, 'b) parse_node
  | Lift_node : {
      f : ('a -> 'b);
      p : ('tok, 'a) parser;
      node : ('tok, 'a) parse_node;
      value : 'b;
      len : int;
    } -> ('tok, 'b) parse_node
  | Pratt_parse_node : {
      lookups : ('tok, 'a) lookups;
      pratt_node : ('tok, 'a) pratt_node;
    } -> ('tok, 'a) parse_node

(* Storing the value and total length as constant time access is needed. *)
and ('tok, 'a) pratt_node = {
  info : ('tok, 'a) pratt_info;
  value : 'a;
  token_length : int; (* Length of the token that triggered the creation of the node (can be 0). *)
  total_length : int; (* Length of this node (token_length) + length of sub-nodes. *)
}

and ('tok, 'a) pratt_info =
  | Leaf of 'a
  | Prefix of {
      prec : int;
      f : 'a -> 'a;
      right : ('tok, 'a) pratt_node;
    }
  | Infix of {
      prec : int;
      f : 'a -> 'a -> 'a;
      left : ('tok, 'a) pratt_node;
      right : ('tok, 'a) pratt_node;
    }
  | Postfix of {
      prec : int;
      f : 'a -> 'a;
      left : ('tok, 'a) pratt_node;
    }
  | Combinators of {
      parser : ('tok, 'a) parser;
      parse_node : ('tok, 'a) parse_node;
    }

and ('tok, 'a) state = {
  lookups : ('tok, 'a) lookups;
  lexer : 'tok Lexer.t;
}

and ('tok, 'a) lookups = {
  prefixes : ('tok -> ('tok, 'a) prefix);
  empty_prefix : ('tok, 'a) prefix;
  infixes : ('tok -> ('tok, 'a) infix);
}

and ('tok, 'a) state_f = ('tok, 'a) state -> ('tok, 'a) pratt_info * ('tok, 'a) state

and ('tok, 'a) prefix = ('tok, 'a) state_f option

and ('tok, 'a) infix = int * (('tok, 'a) pratt_node -> ('tok, 'a) state_f)

module Node = struct
  let value = function
    | Satisfy_node {value; _} | App_node {value; _} | Lift_node {value; _} -> value
    | Pratt_parse_node {pratt_node; _} -> pratt_node.value

  let length : type a. ('tok, a) parse_node -> int = function
    | Satisfy_node {len; _} | App_node {len; _} | Lift_node {len; _} -> len
    | Pratt_parse_node {pratt_node; _} -> pratt_node.total_length

  let satisfy value ~len = Satisfy_node {value; len}

  let pratt ~lookups pratt_node = Pratt_parse_node {lookups; pratt_node}

  let lift ~f ~p node = Lift_node {f; p; node; value = f (value node); len = length node}

  let app ~p ~q left right =
    let value = value left (value right) in
    App_node {p; q; left; right; value; len = length left + length right}
end

let make_pratt_node info ~token_length =
  let v {value; _} = value in
  let l {total_length; _} = total_length in
  let value, sub_length = match info with
    | Leaf v -> v, 0
    | Prefix {f; right; _} -> f (v right), l right
    | Infix {f; left; right; _} -> f (v left) (v right), l left + l right
    | Postfix {f; left; _} -> f (v left), l left
    | Combinators {parse_node; _} -> Node.value parse_node, Node.length parse_node
  in
  {info; value; token_length; total_length = token_length + sub_length}

let rec parse_infix ~prec left ({lookups = {infixes; _}; lexer; _} as state) =
  let token, token_length, lexer = lexer |> Lexer.next in
  let next_prec, infix = infixes token in
  if next_prec >= prec then
    (* The next operator has lower precedence so can't be parsed at this level of the parse tree.
       It will be parsed higher up in the tree (unless it is Infix.unknown). *)
    left, state
  else
    let state = {state with lexer} in (* Advance the state, moving past the token. *)
    let node_info, state = infix left state in
    let node = make_pratt_node node_info ~token_length in
    state |> parse_infix ~prec node

let parse_prefix ~prec ({lookups = {prefixes; empty_prefix; _}; lexer; _} as state) =
  let token, token_length, lexer = lexer |> Lexer.next in
  match prefixes token with
  | Some prefix -> (* Known prefix operator. *)
    let state = {state with lexer} in (* Advance the state, moving past the token. *)
    let info, state = prefix state in
    state |> parse_infix ~prec (make_pratt_node info ~token_length)
  | None -> (* Unknown prefix operator, try to use the empty prefix. *)
    match empty_prefix with
    | Some prefix ->
      (* The state isn't advanced here as the token hasn't been used. *)
      let info, state = prefix state in
      state |> parse_infix ~prec (make_pratt_node info ~token_length:0)
    | None ->
      failwith ("Unknown prefix at pos " ^ string_of_int (Lexer.pos state.lexer))

let pratt_parse state =
  state |> parse_prefix ~prec:max_int (* Begin parsing with the lowest precedence. *)

let rec parse : type tok a. lexer:tok Lexer.t -> (tok, a) parser ->
  (tok, a) parse_node * tok Lexer.t = fun ~lexer parser ->
  let fail s = failwith @@ s ^ " failed at pos " ^ (string_of_int @@ Lexer.pos lexer) ^ "." in
  match parser with
  | Satisfy f ->
    let tok, len, lexer = lexer |> Lexer.next in
    begin match f tok with
      | Some v -> Node.satisfy v ~len, lexer
      | None -> fail "Satisfy"
    end
  | Fix f ->
    parse (f (Fix f)) ~lexer
  | Pratt lookups ->
    let parse_tree, {lexer; _} = pratt_parse {lookups; lexer} in
    Node.pratt ~lookups parse_tree, lexer
  | Lift (f, p) ->
    let node, lexer = parse p ~lexer in
    Node.lift ~f ~p node, lexer
  | App (p, q) ->
    let left, lexer = parse p ~lexer in
    let right, lexer = parse q ~lexer in
    Node.app ~p ~q left right, lexer

module Combinators = struct
  let eat token = Satisfy (fun token' -> if token = token' then Some token else None)

  let satisfy f = Satisfy f

  let fix f = Fix f

  let (<$>) f p = Lift (f, p)

  let (<*>) p q = App (p, q)

  let ( *>) p q = (fun _ r -> r) <$> p <*> q

  let (<* ) p q = (fun r _ -> r) <$> p <*> q
end

module Infix = struct
  let left prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~prec in
      Infix {prec; f; left; right}, state

  let right prec f =
    prec,
    fun left state ->
      (* Use prec + 1 to make the parse tree right associative. *)
      let right, state = state |> parse_prefix ~prec:(prec + 1) in
      Infix {prec = prec + 1; f; left; right}, state

  let postfix ?(prec=(-2)) f =
    prec,
    fun left state -> Postfix {prec; f; left}, state

  (* This can't be parsed by the pratt parser as it has the lowest possible precedence. *)
  let unknown = max_int, fun _left _state -> assert false
end

module Prefix = struct
  let some x = Some x

  let return v = some @@ fun state -> Leaf v, state

  let unary ?(prec=(-1)) f = some @@ fun state ->
    let right, state = state |> parse_prefix ~prec in
    Prefix {prec; f; right}, state

  let custom parser = some @@ fun ({lexer; _} as state) ->
    let parse_node, lexer = parse parser ~lexer in
    (* TODO need to carry across right nodes. *)
    Combinators {parser; parse_node}, {state with lexer}

  let unknown = None
end

let pratt_parser ?(empty_prefix=Prefix.unknown) ?(infixes=fun _ -> Infix.unknown) prefixes =
  Pratt {prefixes; empty_prefix; infixes}

module Non_incremental = struct
  let build_parse_tree ~lexer parser = parse parser ~lexer |> fst

  let run parser ~lexer = build_parse_tree parser ~lexer |> Node.value
end

module Incremental = struct
  type ('tok, 'a) t = {
    parser : ('tok, 'a) parser;
    parse_tree : ('tok, 'a) parse_node;
  }

  type change_loc = {
    start : int;
    added : int;
    removed : int;
  }

  let make ~lexer parser =
    let parse_tree = Non_incremental.build_parse_tree parser ~lexer in
    Node.value parse_tree, {parser; parse_tree}

  let rec update_parse : type a. change_loc -> lexer:'tok Lexer.t -> pos:int ->
    parser:('tok, a) parser -> ('tok, a) parse_node -> ('tok, a) parse_node * 'tok Lexer.t =
    fun ({start; added; removed} as change) ~lexer ~pos ~parser -> function
      | Satisfy_node {len; _} as node ->
        assert (start >= pos && start <= pos + len);
        (* If the start position is just after the node then the node is just returned, which
           saves lexing and parsing a token. *)
        if start = pos + len then node, lexer
        else parse parser ~lexer:(Lexer.move_to pos lexer)
      | Pratt_parse_node {lookups; pratt_node; _} ->
        let pratt_node, lexer = update_pratt change ~lexer ~pos ~lookups pratt_node in
        Node.pratt ~lookups pratt_node, lexer
      | Lift_node {f; p; node; _} ->
        let node, lexer = node |> update_parse change ~lexer ~pos ~parser:p in
        Node.lift ~f ~p node, lexer
      | App_node {p; q; left; right; _} ->
        let mid_pos = pos + Node.length left in
        (* If the start position is past the mid position then only the right needs to be updated.
           If the start position is before the mid position then the left needs to be updated first
           and then possibly the right as well. If the start position is equal to the mid position
           then the left only needs to be updated if tokens have been added, as these could have
           been added to the end of the left parse tree. *)
        if start > mid_pos || (start = mid_pos && added = 0) then (* Update right only. *)
          let right, lexer = right |> update_parse change ~lexer ~pos:mid_pos ~parser:q in
          Node.app ~p ~q left right, lexer
        else (* Update left first. *)
          let left, lexer = left |> update_parse change ~lexer ~pos ~parser:p in
          let pos = Lexer.pos lexer in
          (* We now need to calculate the lengths that still need to be added to and removed from
             the right sub-node. The remaining length to be added to the right = the original
             length to be added - the length added to the left (new position - start position). *)
          let added = max 0 (added - (pos - start)) in
          (* The remaining length to be removed from the right = the original length to be removed
             - the length removed from the left. The reason the length removed from the left is
             mid_pos - start is because that is the length from the start position to the mid
             position, which all must have been removed and replaced when the left was updated. *)
          let removed = max 0 (removed - (mid_pos - start)) in
          if added = 0 && removed = 0 then (* Only left needed to be updated. *)
            Node.app ~p ~q left right, lexer
          else (* Update right as well. *)
            let new_change = {start = pos; added; removed} in
            let right, lexer = right |> update_parse new_change ~lexer ~pos ~parser:q in
            Node.app ~p ~q left right, lexer

  and update_pratt : type a. change_loc -> lexer:'tok Lexer.t -> pos:int ->
    lookups:('tok, a) lookups -> ('tok, a) pratt_node -> ('tok, a) pratt_node * 'tok Lexer.t =
    fun ({start; added; removed} as change) ~lexer ~pos ~lookups pratt_node ->
      let rec incr_parse ~prec ~pos:left_pos ({info; token_length; _} as node) =
        (* The position passed into this function (left_pos) is the position that the node starts
           at, so the position of the token that triggered this node will be equal to this position
           plus the total length of the left sub-node (if there is one). For leaf, prefix and
           combinator nodes, this is just the left position. *)
        let token_start_pos = match info with
          | Leaf _ | Prefix _ | Combinators _ -> left_pos
          | Infix {left; _} | Postfix {left; _} -> left_pos + left.total_length
        in
        let token_end_pos = token_start_pos + token_length in
        if start < token_start_pos then (* Update left. *)
          match info with
          | Infix {left; _} | Postfix {left; _} ->
            left |> incr_parse ~prec ~pos:left_pos
          | Leaf _ | Prefix _ | Combinators _ -> assert false
        else if start >= token_end_pos && start > token_start_pos then (* Update right. *)
          (* The check for start > token_start_pos handles the case where the empty prefix triggered
             the parse, so token_length = 0. This should not update right but instead re-parse from
             before the empty prefix in case the modification means it is no longer taken. *)
          let go_right ~prec:prec' make_info right =
            (* Update right using the precedence of the current node (prec'). *)
            let right, state = right |> incr_parse ~prec:prec' ~pos:token_end_pos in
            let node = make_pratt_node (make_info right) ~token_length in
            (* Start parsing using the precedence of the parent node (prec). *)
            state |> parse_infix ~prec node
          in
          match info with
          | Prefix {prec; f; right; _} ->
            right |> go_right ~prec (fun right -> Prefix {prec; f; right})
          | Infix {prec; f; left; right; _} ->
            right |> go_right ~prec (fun right -> Infix {prec; f; left; right})
          | Leaf _ | Postfix _ ->
            (* Needing to update to the right of a leaf or postfix node must mean the start position
               is at the very right of the tree, so start parsing at the end of the token. *)
            let state = {lookups; lexer = lexer |> Lexer.move_to token_end_pos} in
            state |> parse_infix ~prec node
          | Combinators {parser; parse_node} ->
            let pos = token_end_pos in
            let parse_node, lexer = parse_node |> update_parse change ~lexer ~pos ~parser in
            let node = make_pratt_node (Combinators {parser; parse_node}) ~token_length in
            node, {lookups; lexer}
        else
          let state = {lookups; lexer = lexer |> Lexer.move_to token_start_pos} in
          match info with
          | Leaf _ | Prefix _ | Combinators _ -> (* Token corresponded to a prefix operator. *)
            state |> parse_prefix ~prec
          | Infix {left; _} | Postfix {left; _} -> (* Token corresponded to an infix operator. *)
            state |> parse_infix ~prec left
      in
      let pratt_node, state = pratt_node |> incr_parse ~pos ~prec:max_int in
      pratt_node, state.lexer

  let update ~start ~added ~removed ~lexer ({parser; parse_tree} as t) =
    let change = {start; added; removed} in
    let parse_tree, _ = parse_tree |> update_parse change ~lexer ~pos:0 ~parser in
    Node.value parse_tree, {t with parse_tree}
end
