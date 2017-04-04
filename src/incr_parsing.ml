module Lexer = Incr_lexing.Lexer

(*module Bal_tree = struct
  module Zipped = Zipped_trees (F_array) (F_array)

  include Append_tree (Zipped)

  let parse_tree t = t |> underlying_tree |> Zipped.left

  let ast t = t |> underlying_tree |> Zipped.right
end*)

module Make (Tag : Tagging.Tag) = struct
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

  (* The value and total length are stored in the node as constant time access is needed. *)
  and ('tok, 'a) pratt_node = {
    info : ('tok, 'a) pratt_info;
    token_length : int; (* Length of the token that triggered the creation of the node.
                           Might be 0 if triggered by the empty prefix. *)
    total_length : int; (* Length of this node (token_length) + length of sub-nodes. *)
    type_tag : 'a Tag.t;
  }

  and ('tok, 'a) pratt_info =
    | Leaf of 'a
    | Prefix of {
        prec : int;
        f : 'a -> 'a;
        right : ('tok, 'a) pratt_node;
        value : 'a;
      }
    | Infix of {
        prec : int;
        f : 'a -> 'a -> 'a;
        left : ('tok, 'a) pratt_node;
        right : ('tok, 'a) pratt_node;
        value : 'a;
      }
    | Postfix of {
        prec : int;
        f : 'a -> 'a;
        left : ('tok, 'a) pratt_node;
        value : 'a;
      }
    | Combinators of {
        parser : ('tok, 'a) parser;
        parse_node : ('tok, 'a) parse_node;
      }

  and 'tok fragment =
    | Full : ('tok, 'a) pratt_node -> 'tok fragment
    | Partial : ('tok, 'a) pratt_node -> 'tok fragment

  and 'tok fragments_stack = {
    fragments : 'tok fragment list;
    right_pos : int;
  }

  and ('tok, 'a) lookups = {
    prefixes : ('tok -> ('tok, 'a) prefix);
    empty_prefix : ('tok, 'a) prefix;
    infixes : ('tok -> ('tok, 'a) infix);
    tag : 'a Tag.t;
  }

  and ('tok, 'a) state = {
    lookups : ('tok, 'a) lookups;
    lexer : 'tok Lexer.t;
    reuse : 'tok fragments_stack;
  }

  and ('tok, 'a) state_f = ('tok, 'a) state -> ('tok, 'a) pratt_info * ('tok, 'a) state

  and ('tok, 'a) prefix = ('tok, 'a) state_f option

  and ('tok, 'a) infix = int * (('tok, 'a) pratt_node -> ('tok, 'a) state_f)

  module Node = struct
    let rec value = function
      | Satisfy_node {value; _} | App_node {value; _} | Lift_node {value; _} -> value
      | Pratt_parse_node {pratt_node; _} -> pratt_value pratt_node

    and pratt_value {info; _} = match info with
      | Leaf value | Prefix {value; _} | Infix {value; _} | Postfix {value; _} -> value
      | Combinators {parse_node; _} -> value parse_node

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

  module Pratt = struct
    let value = Node.pratt_value

    let leaf v = Leaf v

    let prefix ~prec ~f ~right = Prefix {prec; f; right; value = f (value right)}

    let infix ~prec ~f ~left ~right =
      Infix {prec; f; left; right; value = f (value left) (value right)}

    let postfix ~prec ~f ~left = Postfix {prec; f; left; value = f (value left)}

    let make_node info ~token_length ~tag =
      let l {total_length; _} = total_length in
      let sub_length = match info with
        | Leaf _ -> 0
        | Prefix {right; _} -> l right
        | Infix {left; right; _} -> l left + l right
        | Postfix {left; _} -> l left
        | Combinators {parse_node; _} -> Node.length parse_node
      in
      {info; token_length; total_length = token_length + sub_length; type_tag = tag}
  end

  module Fragments_stack = struct
    let empty end_pos = {fragments = []; right_pos = end_pos}

    let push_internal fragment ~pos fragments =
      {fragments = fragment::fragments; right_pos = pos}

    let push_full node ~pos {fragments; right_pos} =
      assert (pos = right_pos - node.total_length);
      fragments |> push_internal (Full node) ~pos

    let push_partial node ~pos {fragments; right_pos} =
      begin match node.info with
        | Postfix _ -> assert (pos = right_pos - node.token_length)
        | Infix {right; _} -> assert (pos = right_pos - right.total_length - node.token_length)
        | _ -> assert false
      end;
      fragments |> push_internal (Partial node) ~pos

    (*let check_for_node ({iter; right_nodes; right_pos; _} as state) =
      let length_right = function
        | Leaf _ | Prefix _ | Combinators _ as node -> length_pratt node
        | Infix {right; _} -> length_pratt right + 1
        | Postfix _ -> 1
      in
      let pos = Iter_wrapper.pos iter in
      match right_nodes with
      | node::right_nodes when pos = right_pos ->
        let iter = iter |> Iter_wrapper.skip (length_right node) in
        Some (node, {state with iter; right_nodes; right_pos=pos})
      | _ -> None*)
  end

  let rec parse_infix ~prec left ({lookups = {infixes; tag; _}; lexer; _} as state) =
    let token, token_length, lexer = lexer |> Lexer.next in
    let next_prec, infix = infixes token in
    if next_prec >= prec then
      (* The next operator has lower precedence so can't be parsed at this level of the parse tree.
         It will be parsed higher up in the tree (unless it is Infix.unknown). *)
      left, state
    else
      let state = {state with lexer} in (* Advance the state, moving past the token. *)
      let node_info, state = infix left state in
      let node = Pratt.make_node node_info ~token_length ~tag in
      state |> parse_infix ~prec node

  let parse_prefix ~prec ({lookups = {prefixes; empty_prefix; tag; _}; lexer; _} as state) =
    let token, token_length, lexer = lexer |> Lexer.next in
    match prefixes token with
    | Some prefix -> (* Known prefix operator. *)
      let state = {state with lexer} in (* Advance the state, moving past the token. *)
      let info, state = prefix state in
      state |> parse_infix ~prec (Pratt.make_node info ~token_length ~tag)
    | None -> (* Unknown prefix operator, try to use the empty prefix. *)
      match empty_prefix with
      | Some prefix ->
        (* The state isn't advanced here as the token hasn't been used. *)
        let info, state = prefix state in
        state |> parse_infix ~prec (Pratt.make_node info ~token_length:0 ~tag)
      | None ->
        failwith ("Unknown prefix at pos " ^ string_of_int (Lexer.pos state.lexer))

  let pratt_parse state =
    state |> parse_prefix ~prec:max_int (* Begin parsing with the lowest precedence. *)

  let rec parse : type tok a. lexer:tok Lexer.t -> reuse:tok fragments_stack ->
    (tok, a) parser -> (tok, a) parse_node * tok Lexer.t * tok fragments_stack =
    fun ~lexer ~reuse parser ->
      match parser with
      | Satisfy f ->
        let tok, len, lexer = lexer |> Lexer.next in
        begin match f tok with
          | Some v -> Node.satisfy v ~len, lexer, reuse
          | None -> failwith ("Satisfy failed at pos " ^ string_of_int (Lexer.pos lexer) ^ ".")
        end
      | Fix f ->
        parse (f (Fix f)) ~lexer ~reuse
      | Pratt lookups ->
        let parse_tree, {lexer; _} = pratt_parse {lookups; lexer; reuse} in
        Node.pratt ~lookups parse_tree, lexer, reuse
      | Lift (f, p) ->
        let node, lexer, reuse = parse p ~lexer ~reuse in
        Node.lift ~f ~p node, lexer, reuse
      | App (p, q) ->
        let left, lexer, reuse = parse p ~lexer ~reuse in
        let right, lexer, reuse = parse q ~lexer ~reuse in
        Node.app ~p ~q left right, lexer, reuse

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
        Pratt.infix ~prec ~f ~left ~right, state

    let right prec f =
      prec,
      fun left state ->
        (* Use prec + 1 to make the parse tree right associative. *)
        let right, state = state |> parse_prefix ~prec:(prec + 1) in
        Pratt.infix ~prec:(prec + 1) ~f ~left ~right, state

    let postfix ?(prec = -2) f =
      prec,
      fun left state -> Pratt.postfix ~prec ~f ~left, state

    (* This can't be parsed by the pratt parser as it has the lowest possible precedence. *)
    let unknown = max_int, fun _left _state -> assert false
  end

  module Prefix = struct
    let some x = Some x

    let return v = some @@ fun state -> Pratt.leaf v, state

    let unary ?(prec = -1) f = some @@ fun state ->
      let right, state = state |> parse_prefix ~prec in
      Pratt.prefix ~prec ~f ~right, state

    let custom parser = some @@ fun ({lexer; reuse; _} as state) ->
      let parse_node, lexer, reuse = parse parser ~lexer ~reuse in
      Combinators {parser; parse_node}, {state with lexer; reuse}

    let unknown = None
  end

  let pratt_parser ?(infixes = fun _ -> Infix.unknown) ?(prefixes = fun _ -> Prefix.unknown)
      ?(empty_prefix = Prefix.unknown) tag =
    Pratt {prefixes; empty_prefix; infixes; tag}

  module Parse_tree = struct
    type 'a t = E : ('tok, 'a) parse_node -> 'a t

    let of_parse_node node = E node

    let to_ast (E node) = Node.value node

    let length (E node) = Node.length node
  end

  module Non_incremental = struct
    let build_parse_node parser ~lexer =
      let parse_node, _, _ = parse parser ~lexer ~reuse:(Fragments_stack.empty 0) in
      parse_node

    let run parser ~lexer = build_parse_node parser ~lexer |> Parse_tree.of_parse_node
  end

  module Incremental = struct
    type ('tok, 'a) t = {
      parser : ('tok, 'a) parser;
      parse_node : ('tok, 'a) parse_node;
    }

    type change_loc = {
      start : int;
      added : int;
      removed : int;
    }

    let make parser ~lexer =
      let parse_node = Non_incremental.build_parse_node parser ~lexer in
      {parser; parse_node}

    let parse_tree {parse_node; _} = Parse_tree.of_parse_node parse_node

    let rec update_parse : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok fragments_stack ->
      pos:int -> parser:('tok, a) parser -> ('tok, a) parse_node ->
      ('tok, a) parse_node * 'tok Lexer.t * 'tok fragments_stack =
      fun ({start; added; removed} as change) ~lexer ~reuse ~pos ~parser -> function
        | Satisfy_node {len; _} as node ->
          assert (start >= pos && start <= pos + len);
          (* If the start position is just after the node then the node is just returned, which
             saves lexing and parsing a token. *)
          if start = pos + len then node, lexer, reuse
          else parse parser ~lexer:(Lexer.move_to pos lexer) ~reuse
        | Pratt_parse_node {lookups; pratt_node; _} ->
          let node, lexer, reuse = update_pratt change ~lexer ~reuse ~pos ~lookups pratt_node in
          Node.pratt ~lookups node, lexer, reuse
        | Lift_node {f; p; node; _} ->
          let node, lexer, reuse = node |> update_parse change ~lexer ~reuse ~pos ~parser:p in
          Node.lift ~f ~p node, lexer, reuse
        | App_node {p; q; left; right; _} ->
          let mid_pos = pos + Node.length left in
          (* If the start position is past the mid position then only the right needs to be updated.
             If the start position is before the mid position then the left needs to be updated
             first and then possibly the right as well. If the start position is equal to the mid
             position then the left only needs to be updated if tokens have been added, as these
             could have been added to the end of the left parse tree. *)
          if start > mid_pos || (start = mid_pos && added = 0) then (* Update right only. *)
            let right, lexer, reuse =
              right |> update_parse change ~lexer ~reuse ~pos:mid_pos ~parser:q in
            Node.app ~p ~q left right, lexer, reuse
          else (* Update left first. *)
            let left, lexer, reuse = left |> update_parse change ~lexer ~reuse ~pos ~parser:p in
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
              Node.app ~p ~q left right, lexer, reuse
            else (* Update right as well. *)
              let new_change = {start = pos; added; removed} in
              let right, lexer, reuse =
                right |> update_parse new_change ~lexer ~reuse ~pos ~parser:q in
              Node.app ~p ~q left right, lexer, reuse

    and update_pratt : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok fragments_stack ->
      pos:int -> lookups:('tok, a) lookups -> ('tok, a) pratt_node ->
      ('tok, a) pratt_node * 'tok Lexer.t * 'tok fragments_stack =
      fun ({start; added; removed} as change) ~lexer ~reuse ~pos ~lookups pratt_node ->
        let {tag; _} = lookups in
        let change_offset = added - removed in
        let rec incr_parse ~reuse ~prec ~pos:left_pos ({info; token_length; _} as node) =
          (* The position passed into this function (left_pos) is the position that the node starts
             at, so the position of the token that triggered this node will be equal to this
             position plus the total length of the left sub-node (if there is one). For leaf, prefix
             and combinator nodes, this is just the left position. *)
          let token_start_pos = match info with
            | Leaf _ | Prefix _ | Combinators _ -> left_pos
            | Infix {left; _} | Postfix {left; _} -> left_pos + left.total_length
          in
          let token_end_pos = token_start_pos + token_length in
          if start < token_start_pos then (* Update left. *)
            match info with
            | Infix {left; _} | Postfix {left; _} ->
              let stack_pos = token_start_pos + change_offset in
              let reuse = reuse |> Fragments_stack.push_partial node ~pos:stack_pos in
              left |> incr_parse ~reuse ~prec ~pos:left_pos
            | Leaf _ | Prefix _ | Combinators _ -> assert false
          else if start >= token_end_pos && start > token_start_pos then (* Update right. *)
            (* The check for start > token_start_pos handles the empty prefix, so token_length = 0.
               This should not update right but instead re-parse from before the empty prefix in
               case the modification means it is no longer taken. *)
            let go_right ~prec:prec' make_info right =
              (* Update right using the precedence of the current node (prec'). *)
              let right, state = right |> incr_parse ~reuse ~prec:prec' ~pos:token_end_pos in
              let node = Pratt.make_node (make_info ~right) ~token_length ~tag in
              (* Start parsing using the precedence of the parent node (prec). *)
              state |> parse_infix ~prec node
            in
            match info with
            | Prefix {prec; f; right; _} ->
              right |> go_right ~prec (Pratt.prefix ~prec ~f)
            | Infix {prec; f; left; right; _} ->
              right |> go_right ~prec (Pratt.infix ~prec ~f ~left)
            | Leaf _ | Postfix _ ->
              (* Needing to update to the right of a leaf or postfix node means the start position
                 is at the very right of the tree, so start parsing at the end of the token. *)
              let lexer = lexer |> Lexer.move_to token_end_pos in
              let state = {lookups; lexer; reuse} in
              state |> parse_infix ~prec node
            | Combinators {parser; parse_node} ->
              (* Start incrementally parsing after the token. *)
              let parse_node, lexer, reuse =
                parse_node |> update_parse change ~lexer ~reuse ~pos:token_end_pos ~parser in
              let node = Pratt.make_node (Combinators {parser; parse_node}) ~token_length ~tag in
              (* Move the lexer to after the node and parse. *)
              let lexer = lexer |> Lexer.move_to (token_start_pos + node.total_length) in
              {lookups; lexer; reuse} |> parse_infix ~prec node
          else (* Start parsing at this node's position. *)
            let lexer = lexer |> Lexer.move_to token_start_pos in
            let state = {lookups; lexer; reuse} in
            match info with
            | Leaf _ | Prefix _ | Combinators _ -> (* Token corresponded to a prefix operator. *)
              state |> parse_prefix ~prec
            | Infix {left; _} | Postfix {left; _} -> (* Token corresponded to an infix operator. *)
              state |> parse_infix ~prec left
        in
        let node, {lexer; reuse; _} = pratt_node |> incr_parse ~prec:max_int ~reuse ~pos in
        node, lexer, reuse

    let update ~start ~added ~removed ~lexer ({parser; parse_node} as t) =
      if start < 0 || added < 0 || removed < 0 then
        invalid_arg "The arguments start, added and removed must all be non-negative."
      else if added = 0 && removed = 0 then t
      else
        let change = {start; added; removed} in
        let right_pos = Node.length parse_node + added - removed in
        let reuse = Fragments_stack.empty right_pos in
        let parse_node, _, _ = parse_node |> update_parse change ~lexer ~reuse ~pos:0 ~parser in
        {t with parse_node}
  end
end
