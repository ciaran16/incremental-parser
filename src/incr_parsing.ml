module Lexer = Incr_lexing.Lexer

type ('tok, _) parser =
  | Satisfy : ('tok -> 'a option) -> ('tok, 'a) parser
  | Pratt : ('tok, 'a) lookups -> ('tok, 'a) parser
  | Lift : ('a -> 'b) * ('tok, 'a) parser -> ('tok, 'b) parser
  | App : ('tok, 'a -> 'b) parser * ('tok, 'a) parser -> ('tok, 'b) parser
  | Fix : ('tok, 'a) parser Lazy.t -> ('tok, 'a) parser

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

and 'tok dyn_fragment = Dyn : ('tok, 'a) pratt_node * int -> 'tok dyn_fragment

and ('tok, 'a) lookups = {
  prefixes : ('tok -> ('tok, 'a) prefix);
  empty_prefix : ('tok, 'a) prefix;
  infixes : ('tok -> ('tok, 'a) infix);
  tag : 'a Tag.t;
}

and ('tok, 'a) state = {
  lookups : ('tok, 'a) lookups;
  lexer : 'tok Lexer.t;
  reuse : 'tok dyn_fragment list;
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

  let token_start_pos ~left_pos = function
    | Leaf _ | Prefix _ | Combinators _ -> left_pos
    | Infix {left; _} | Postfix {left; _} -> left_pos + left.total_length
end

module Reuse = struct
  type 'tok t = 'tok dyn_fragment list
    (* A list of dynamic pratt nodes where the head node is the leftmost node and the following
       nodes are (grand)parent nodes or nodes further right in the tree. *)

  let empty = []

  let extract_pratt ~left_pos parse_node =
    let rec extract : type a. 'tok t -> left_pos:int -> ('tok, a) parse_node
      -> 'tok t = fun t ~left_pos -> function
      | Satisfy_node _ -> t
      | Lift_node {node; _} -> node |> extract t ~left_pos
      | Pratt_parse_node {pratt_node; _} -> Dyn (pratt_node, left_pos) :: t
      | App_node {left; right; _} ->
        let t = right |> extract t ~left_pos:(left_pos + Node.length left) in
        left |> extract t ~left_pos
    in
    parse_node |> extract [] ~left_pos

  let rec seek ~seek_pos = function
    | [] -> None, []
    | Dyn (node, left_pos)::tl as t ->
      if seek_pos < left_pos then None, t
      else if seek_pos < left_pos + node.total_length then node |> down tl ~left_pos ~seek_pos
      else tl |> seek ~seek_pos

  and down : type a. 'tok t -> left_pos:int -> seek_pos:int -> ('tok, a) pratt_node
    -> 'tok dyn_fragment option * 'tok t = fun t ~left_pos ~seek_pos ({info; _} as node) ->
    let token_start_pos = info |> Pratt.token_start_pos ~left_pos in
    if token_start_pos = seek_pos then Some (Dyn (node, left_pos)), t
    else if seek_pos < token_start_pos then match info with (* Left. *)
      | Leaf _ | Prefix _ | Combinators _ -> assert false
      | Infix {left; _} | Postfix {left; _} ->
        left |> down (Dyn (node, left_pos) :: t) ~left_pos ~seek_pos
    else (* seek_pos > token_start_pos. *)
      let token_end_pos = token_start_pos + node.token_length in
      if seek_pos < token_end_pos then None, t
      else match info with (* Right. *)
        | Leaf _ | Postfix _ -> assert false
        | Prefix {right; _} | Infix {right; _} ->
          right |> down t ~left_pos:token_end_pos ~seek_pos
        | Combinators {parse_node; _} ->
          let o, t' = parse_node |> extract_pratt ~left_pos:token_end_pos |> seek ~seek_pos in
          o, t' @ t

  let create parse_tree ~start ~added ~removed =
    (* We align the original parse tree to the new parse tree by starting with an offset. Then we
       move past all the nodes to the left of the change so only nodes to the right of the change
       can be reused. *)
    let t = parse_tree |> extract_pratt ~left_pos:(added - removed) in
    match t |> seek ~seek_pos:(start + added) with
    | None, t -> t
    | Some hd, tl -> hd::tl

  let seek_right ~seek_pos = function
    | [] -> None, []
    | Dyn ({info; _}, left_pos)::_ as t ->
      if seek_pos < Pratt.token_start_pos info ~left_pos then None, t
      else t |> seek ~seek_pos

  let satisfy_at (type a) seek_pos ~(tag : a Tag.t)
      (check_f : ('tok, a) pratt_node -> ('tok, a) pratt_node option) t :
    ('tok, a) pratt_node option * 'tok t =
    match t |> seek_right ~seek_pos with
    | None, t -> None, t
    | Some (Dyn (node, _) as hd), t ->
      match Tag.compare node.type_tag tag with
      | Tag.Not_equal -> None, hd::t
      | Tag.Equal ->
        match check_f node with
        | None -> None, hd::t
        | Some _ as some -> Printf.printf "Reusing node at position %i.\n" seek_pos; some, t
end

(* NOTE: Currently the end token will always be explicitly read, which is annoying. *)

let rec parse_infix ~prec:parse_prec left ({lexer; reuse; _} as state) =
  let tag = left.type_tag in
  let o, reuse = reuse |> Reuse.satisfy_at (Lexer.pos lexer) ~tag @@ fun {info; token_length; _} ->
    match info with
    | Infix {prec; f; right; _} when prec < parse_prec ->
      let info = Pratt.infix ~prec ~f ~left ~right in
      Some (Pratt.make_node info ~token_length ~tag)
    | Postfix {prec; f; _} when prec < parse_prec ->
      let info = Pratt.postfix ~prec ~f ~left in
      Some (Pratt.make_node info ~token_length ~tag)
    | _ -> None
  in
  match o with
  | Some node -> (* Reusing a node. *)
    let lexer = lexer |> Lexer.skip (node.total_length - left.total_length) in
    {state with lexer; reuse} |> parse_infix ~prec:parse_prec node
  | None -> (* Parsing a node. *)
    let token, token_length, lexer = lexer |> Lexer.next in
    let prec, infix = state.lookups.infixes token in
    if prec >= parse_prec then
      (* The next operator has lower precedence so can't be parsed at this level of the parse tree.
         It will be parsed higher up in the tree (unless it is Infix.unknown). *)
      left, state
    else
      let state = {state with lexer; reuse} in (* Advance the state, moving past the token. *)
      let info, state = infix left state in
      let node = Pratt.make_node info ~token_length ~tag in
      state |> parse_infix ~prec:parse_prec node

let parse_prefix ~prec ({lookups={prefixes; empty_prefix; tag; _}; lexer; reuse} as state) =
  let o, reuse = reuse |> Reuse.satisfy_at (Lexer.pos lexer) ~tag @@ function
    | {info=(Leaf _ | Prefix _ | Combinators _); _} as node -> Some node
    | _ -> None
  in
  let node, state = match o with
    | Some node ->
      let lexer = lexer |> Lexer.skip node.total_length in
      node, {state with lexer; reuse}
    | None ->
      let token, token_length, lexer = lexer |> Lexer.next in
      match prefixes token with
      | Some prefix -> (* Known prefix operator. *)
        let state = {state with lexer} in (* Advance the state, moving past the token. *)
        let info, state = prefix state in
        Pratt.make_node info ~token_length ~tag, state
      | None -> (* Unknown prefix operator, try to use the empty prefix. *)
        match empty_prefix with
        | Some empty_prefix ->
          (* The state isn't advanced here as the token hasn't been used. *)
          let info, state = empty_prefix state in
          Pratt.make_node info ~token_length:0 ~tag, state
        | None ->
          failwith ("Unknown prefix at pos " ^ string_of_int (Lexer.pos state.lexer))
  in
  state |> parse_infix ~prec node

let rec parse : type tok a. lexer:tok Lexer.t -> reuse:tok Reuse.t ->
  (tok, a) parser -> (tok, a) parse_node * tok Lexer.t * tok Reuse.t =
  fun ~lexer ~reuse parser ->
    match parser with
    | Satisfy f ->
      let tok, len, lexer = lexer |> Lexer.next in
      begin match f tok with
        | Some v -> Node.satisfy v ~len, lexer, reuse
        | None -> failwith ("Satisfy failed at pos " ^ string_of_int (Lexer.pos lexer) ^ ".")
      end
    | Fix (lazy p) ->
      parse p ~lexer ~reuse
    | Pratt lookups ->
      let parse_tree, {lexer; _} = {lookups; lexer; reuse} |> parse_prefix ~prec:max_int in
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

  let fix f = let rec p = lazy (f (Fix p)) in Lazy.force p

  let (<$>) f p = Lift (f, p)

  let (<*>) p q = App (p, q)

  let ( *>) p q = (fun _ x -> x) <$> p <*> q

  let (<* ) p q = (fun x _ -> x) <$> p <*> q
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

let pratt_parser ?(prefixes = fun _ -> Prefix.unknown) ?(empty_prefix = Prefix.unknown)
    ?(infixes = fun _ -> Infix.unknown) () =
  Pratt {prefixes; empty_prefix; infixes; tag = Tag.fresh ()}

module Parse_tree = struct
  type 'a t = E : ('tok, 'a) parse_node -> 'a t

  let of_parse_node node = E node

  let to_ast (E node) = Node.value node

  let length (E node) = Node.length node
end

module Non_incremental = struct
  let build_parse_node parser ~lexer =
    let parse_node, _, _ = parse parser ~lexer ~reuse:Reuse.empty in
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

  let rec update_parse : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok Reuse.t ->
    pos:int -> parser:('tok, a) parser -> ('tok, a) parse_node ->
    ('tok, a) parse_node * 'tok Lexer.t * 'tok Reuse.t =
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

  and update_pratt : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok Reuse.t ->
    pos:int -> lookups:('tok, a) lookups -> ('tok, a) pratt_node ->
    ('tok, a) pratt_node * 'tok Lexer.t * 'tok Reuse.t =
    fun ({start; _} as change) ~lexer ~reuse ~pos ~lookups pratt_node ->
      let rec incr_parse ~prec ~left_pos {info; token_length; _} =
        (* The position passed into this function (left_pos) is the position that the node starts
           at, so the position of the token that triggered this node will be equal to this
           position plus the total length of the left sub-node (if there is one). For leaf, prefix
           and combinator nodes, this is just the left position. *)
        let token_start_pos = info |> Pratt.token_start_pos ~left_pos in
        let token_end_pos = token_start_pos + token_length in
        if start <= token_end_pos then (* Update left or current node. *)
          (* We check if start = token_end_pos in case we are updating to the very right of the left
             sub-tree. *)
          match info with
          | Infix {left; _} | Postfix {left; _} ->
            if start <= token_start_pos then (* Update left. *)
              left |> incr_parse ~prec ~left_pos
            else (* Update the current node. *)
              let lexer = lexer |> Lexer.move_to token_start_pos in
              {lookups; lexer; reuse} |> parse_infix ~prec left
          | Leaf _ | Prefix _ | Combinators _ -> (* Update the current node. *)
            let lexer = lexer |> Lexer.move_to token_start_pos in
            {lookups; lexer; reuse} |> parse_prefix ~prec
        else (* Update right. *)
          let tag = lookups.tag in
          let go_right ~prec:prec' make_info right =
            (* Update right using the precedence of the current node (prec'). *)
            let right, state = right |> incr_parse ~prec:prec' ~left_pos:token_end_pos in
            let node = Pratt.make_node (make_info ~right) ~token_length ~tag in
            (* Start parsing using the precedence of the parent node (prec). *)
            state |> parse_infix ~prec node
          in
          match info with
          | Prefix {prec; f; right; _} ->
            right |> go_right ~prec (Pratt.prefix ~prec ~f)
          | Infix {prec; f; left; right; _} ->
            right |> go_right ~prec (Pratt.infix ~prec ~f ~left)
          | Combinators {parser; parse_node} ->
            (* Start incrementally parsing after the token. *)
            let parse_node, lexer, reuse =
              parse_node |> update_parse change ~lexer ~reuse ~pos:token_end_pos ~parser in
            let node = Pratt.make_node (Combinators {parser; parse_node}) ~token_length ~tag in
            (* Move the lexer to after the node and parse. *)
            let lexer = lexer |> Lexer.move_to (token_start_pos + node.total_length) in
            {lookups; lexer; reuse} |> parse_infix ~prec node
          | Leaf _ | Postfix _ -> invalid_arg "Update start position is out of bounds."
      in
      let node, {lexer; reuse; _} = pratt_node |> incr_parse ~prec:max_int ~left_pos:pos in
      node, lexer, reuse

  let update ~start ~added ~removed ~lexer ({parser; parse_node} as t) =
    if start < 0 || added < 0 || removed < 0 then
      invalid_arg "The arguments start, added and removed must all be non-negative."
    else if added = 0 && removed = 0 then t
    else
      let change = {start; added; removed} in
      let reuse = Reuse.create parse_node ~start ~added ~removed in
      let parse_node, _, _ = parse_node |> update_parse change ~lexer ~reuse ~pos:0 ~parser in
      {t with parse_node}
end
