module Lexer = Incr_lexing.Lexer

type ('tok, _) parse_node =
  | Satisfy : {
      f : 'tok -> 'a option;
      on_error : 'a option;
      value : 'a;
      length : int;
    } -> ('tok, 'a) parse_node
  | App : {
      left : ('tok, 'a -> 'b) parse_node;
      right : ('tok, 'a) parse_node;
      value : 'b;
      length : int;
    } -> ('tok, 'b) parse_node
  | Lift : {
      f : ('a -> 'b);
      node : ('tok, 'a) parse_node;
      value : 'b;
      length : int;
    } -> ('tok, 'b) parse_node
  | Pratt_parse : {
      lookups : ('tok, 'a) lookups;
      pratt_node : ('tok, 'a) pratt_node;
    } -> ('tok, 'a) parse_node

(* The value and total length are stored in the node as constant time access is needed. *)
and ('tok, 'a) pratt_node = {
  value : 'a;
  info : ('tok, 'a) pratt_info;
  token_length : int; (* Length of the token that triggered the creation of the node.
                         This might be 0 if the parse was triggered by the empty prefix. *)
  total_length : int; (* Length of this node (token_length) + length of sub-nodes. *)
  type_tag : 'a Tag.t;
}

and ('tok, 'a) pratt_info =
  | Leaf
  | Prefix of {
      parse_prec : int;
      f : 'a -> 'a;
      right : ('tok, 'a) pratt_node;
    }
  | Infix of {
      prec : int; (* The precedence of the operator. *)
      parse_prec : int; (* The precedence to parse at (prec + 1 for right associative infixes). *)
      f : 'a -> 'a -> 'a;
      left : ('tok, 'a) pratt_node;
      right : ('tok, 'a) pratt_node;
    }
  | Postfix of {
      prec : int;
      f : 'a -> 'a;
      left : ('tok, 'a) pratt_node;
    }
  | Combinators of ('tok, 'a) parse_node

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

and 'tok dyn_fragment = Dyn : ('tok, 'a) pratt_node * int -> 'tok dyn_fragment

and ('tok, 'a) state_f = ('tok, 'a) state -> (('tok, 'a) pratt_info * 'a) * ('tok, 'a) state

and ('tok, 'a) prefix = ('tok, 'a) state_f option

and ('tok, 'a) infix = int * (('tok, 'a) pratt_node -> ('tok, 'a) state_f)

type ('tok, 'a) parser = lexer:'tok Lexer.t -> reuse:'tok dyn_fragment list ->
  ('tok, 'a) parse_node * 'tok Lexer.t * 'tok dyn_fragment list

module Node = struct
  let value = function
    | Satisfy {value; _} | App {value; _} | Lift {value; _} -> value
    | Pratt_parse {pratt_node; _} -> pratt_node.value

  let length : type a. ('tok, a) parse_node -> int = function
    | Satisfy {length; _} | App {length; _} | Lift {length; _} -> length
    | Pratt_parse {pratt_node; _} -> pratt_node.total_length

  let satisfy ~f ~on_error value ~length = Satisfy {f; on_error; value; length}

  let pratt ~lookups pratt_node = Pratt_parse {lookups; pratt_node}

  let lift ~f node = Lift {f; node; value = f (value node); length = length node}

  let app left right =
    let value = value left @@ value right in
    App {left; right; value; length = length left + length right}
end

module Pratt = struct
  let leaf v = Leaf, v

  let prefix ~parse_prec ~f ~right = Prefix {parse_prec; f; right}, f right.value

  let infix ~prec ~parse_prec ~f ~left ~right =
    Infix {prec; parse_prec; f; left; right}, f left.value right.value

  let postfix ~prec ~f ~left = Postfix {prec; f; left}, f left.value

  let make_node (info, value) ~token_length ~tag:type_tag =
    let total_length = token_length + match info with
      | Leaf -> 0
      | Prefix {right; _} -> right.total_length
      | Infix {left; right; _} -> left.total_length + right.total_length
      | Postfix {left; _} -> left.total_length
      | Combinators parse_node -> Node.length parse_node
    in
    {value; info; token_length; total_length; type_tag}

  let token_start_pos ~left_pos = function
    | Leaf | Prefix _ | Combinators _ -> left_pos
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
      | Satisfy _ -> t
      | Lift {node; _} -> node |> extract t ~left_pos
      | Pratt_parse {pratt_node; _} -> Dyn (pratt_node, left_pos) :: t
      | App {left; right; _} ->
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
      | Leaf | Prefix _ | Combinators _ -> assert false
      | Infix {left; _} | Postfix {left; _} ->
        left |> down (Dyn (node, left_pos) :: t) ~left_pos ~seek_pos
    else (* if seek_pos > token_start_pos *)
      let token_end_pos = token_start_pos + node.token_length in
      if seek_pos < token_end_pos then None, t
      else match info with (* Right. *)
        | Leaf | Postfix _ -> assert false
        | Prefix {right; _} | Infix {right; _} ->
          right |> down t ~left_pos:token_end_pos ~seek_pos
        | Combinators parse_node ->
          let o, t' = parse_node |> extract_pratt ~left_pos:token_end_pos |> seek ~seek_pos in
          o, t' @ t

  let create parse_tree ~start_pos ~added ~removed =
    (* We align the original parse tree to the new parse tree by starting with an offset. Then we
       move past all the nodes to the left of the change so only nodes to the right of the change
       can be reused. *)
    let t = parse_tree |> extract_pratt ~left_pos:(added - removed) in
    match t |> seek ~seek_pos:(start_pos + added) with
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

(* NOTE: Currently the end token will always be explicitly lexed / read, which is annoying. *)

let rec parse_infix ~parse_prec left ({lexer; reuse; _} as state) =
  let tag = left.type_tag in
  let o, reuse = reuse |> Reuse.satisfy_at (Lexer.pos lexer) ~tag @@ fun {info; token_length; _} ->
    match info with
    | Infix {prec; parse_prec=orig_pp; f; right; _} when prec < parse_prec ->
      let info = Pratt.infix ~prec ~parse_prec:orig_pp ~f ~left ~right in
      Some (Pratt.make_node info ~token_length ~tag)
    | Postfix {prec; f; _} when prec < parse_prec ->
      let info = Pratt.postfix ~prec ~f ~left in
      Some (Pratt.make_node info ~token_length ~tag)
    | _ -> None
  in
  match o with
  | Some node -> (* Reusing a node. *)
    let lexer = lexer |> Lexer.skip (node.total_length - left.total_length) in
    {state with lexer; reuse} |> parse_infix ~parse_prec node
  | None -> (* Parsing a node. *)
    let token, token_length, lexer = lexer |> Lexer.next in
    let next_prec, infix = state.lookups.infixes token in
    if next_prec >= parse_prec then
      (* The next operator has the same or lower precedence so can't be parsed at this level of the
         parse tree. It will be parsed higher up in the tree (unless it is Infix.unknown). *)
      left, state
    else
      let state = {state with lexer; reuse} in (* Advance the state, moving past the token. *)
      let info, state = infix left state in
      let node = Pratt.make_node info ~token_length ~tag in
      state |> parse_infix ~parse_prec node

let parse_prefix ~parse_prec ({lookups={prefixes; empty_prefix; tag; _}; lexer; reuse} as state) =
  let o, reuse = reuse |> Reuse.satisfy_at (Lexer.pos lexer) ~tag @@ function
    | {info=(Leaf | Prefix _ | Combinators _); _} as node -> Some node
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
          failwith (Printf.sprintf "Unknown prefix at position %i." (Lexer.pos state.lexer))
  in
  state |> parse_infix ~parse_prec node

module Combinators = struct
  let satisfy ?on_error f = fun ~lexer ~reuse ->
    let token, length, lexer' = lexer |> Lexer.next in
    match f token with
    | Some v -> Node.satisfy ~f ~on_error v ~length, lexer', reuse (* Advance the lexer. *)
    | None ->
      match on_error with
      | Some v ->
        Printf.printf "Using error value for satisfy at position %i." (Lexer.pos lexer);
        Node.satisfy ~f ~on_error v ~length, lexer, reuse (* Do not advance the lexer. *)
      | None -> failwith @@ Printf.sprintf "Satisfy failed at position %i." (Lexer.pos lexer)

  let eat token =
    let tokens_equal token' = if token = token' then Some token else None in
    satisfy tokens_equal ~on_error:token

  let lift f p = fun ~lexer ~reuse ->
    let node, lexer, reuse = p ~lexer ~reuse in
    Node.lift ~f node, lexer, reuse

  let (<$>) = lift

  let (>>|) p f = lift f p

  let app p q = fun ~lexer ~reuse ->
    let left, lexer, reuse = p ~lexer ~reuse in
    let right, lexer, reuse = q ~lexer ~reuse in
    Node.app left right, lexer, reuse

  let (<*>) = app

  let ( *>) p q = (fun _ x -> x) <$> p <*> q

  let (<* ) p q = (fun x _ -> x) <$> p <*> q

  let fix f =
    let rec fixed = lazy (f p)
    and p ~lexer ~reuse = Lazy.force fixed ~lexer ~reuse in
    Lazy.force fixed
end

module Infix = struct
  let left prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~parse_prec:prec in
      Pratt.infix ~prec ~parse_prec:prec ~f ~left ~right, state

  let right prec f =
    prec,
    fun left state ->
      (* Using prec + 1 makes the parse tree right associative. *)
      let right, state = state |> parse_prefix ~parse_prec:(prec + 1) in
      Pratt.infix ~prec ~parse_prec:(prec + 1) ~f ~left ~right, state

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
    let right, state = state |> parse_prefix ~parse_prec:prec in
    Pratt.prefix ~parse_prec:prec ~f ~right, state

  let custom parser = some @@ fun ({lexer; reuse; _} as state) ->
    let parse_node, lexer, reuse = parser ~lexer ~reuse in
    (Combinators parse_node, Node.value parse_node), {state with lexer; reuse}

  let unknown = None
end

let pratt_parser ?(prefixes = fun _ -> Prefix.unknown) ?(empty_prefix = Prefix.unknown)
    ?(infixes = fun _ -> Infix.unknown) ?(tag = Tag.fresh ()) () =
  let lookups = {prefixes; empty_prefix; infixes; tag} in
  fun ~lexer ~reuse ->
    let state = {lookups; lexer; reuse} in
    let parse_tree, {lexer; reuse; _} = state |> parse_prefix ~parse_prec:max_int in
    Node.pratt ~lookups parse_tree, lexer, reuse

module Incremental = struct
  type ('tok, 'a) t = ('tok, 'a) parse_node

  type change_loc = {
    start_pos : int;
    added : int;
    removed : int;
  }

  let make parser ~lexer =
    let parse_node, _, _ = parser ~lexer ~reuse:Reuse.empty in
    parse_node

  let rec update_parse : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok Reuse.t ->
    left_pos:int -> ('tok, a) parse_node ->
    ('tok, a) parse_node * 'tok Lexer.t * 'tok Reuse.t =
    fun ({start_pos; added; removed} as change) ~lexer ~reuse ~left_pos -> function
      | Satisfy {f; on_error; length; _} ->
        assert (start_pos >= left_pos && start_pos <= left_pos + length);
        Combinators.satisfy f ?on_error ~lexer:(Lexer.move_to left_pos lexer) ~reuse
      | Pratt_parse {lookups; pratt_node} ->
        let node, lexer, reuse = update_pratt change ~lexer ~reuse ~left_pos ~lookups pratt_node in
        Node.pratt ~lookups node, lexer, reuse
      | Lift {f; node; _} ->
        let node, lexer, reuse = node |> update_parse change ~lexer ~reuse ~left_pos in
        Node.lift ~f node, lexer, reuse
      | App {left; right; _} ->
        let mid_pos = left_pos + Node.length left in
        (* If the start position is past the mid position then only the right needs to be updated.
           If the start position is equal to or before the mid position then the left needs to be
           updated first and then possibly the right as well. The reason we also check if they're
           equal is because of the longest match rule used lexing, which means the token to the
           left could have changed, and also because errors can cause Satisfy nodes to have a
           length of zero. *)
        if start_pos > mid_pos then (* Update right only. *)
          let right, lexer, reuse = right |> update_parse change ~lexer ~reuse ~left_pos:mid_pos in
          Node.app left right, lexer, reuse
        else (* Update left first. *)
          let left, lexer, reuse = left |> update_parse change ~lexer ~reuse ~left_pos in
          let left_pos = Lexer.pos lexer in
          (* We now need to calculate the lengths that still need to be added to and removed from
             the right sub-node. The remaining length to be added to the right = the original
             length to be added - the length added to the left (new position - start position). *)
          let added = max 0 (added - (left_pos - start_pos)) in
          (* The remaining length to be removed from the right = the original length to be removed
             - the length removed from the left. The reason the length removed from the left is
             mid_pos - start is because that is the length from the start position to the mid
             position, which all must have been removed and replaced when the left was updated. *)
          let removed = max 0 (removed - (mid_pos - start_pos)) in
          if added = 0 && removed = 0 then (* Only left needed to be updated. *)
            Node.app left right, lexer, reuse
          else (* Update right as well. *)
            let new_change = {start_pos = left_pos; added; removed} in
            let right, lexer, reuse = right |> update_parse new_change ~lexer ~reuse ~left_pos in
            Node.app left right, lexer, reuse

  and update_pratt : type a. change_loc -> lexer:'tok Lexer.t -> reuse:'tok Reuse.t ->
    left_pos:int -> lookups:('tok, a) lookups -> ('tok, a) pratt_node ->
    ('tok, a) pratt_node * 'tok Lexer.t * 'tok Reuse.t =
    fun ({start_pos; _} as change) ~lexer ~reuse ~left_pos ~lookups pratt_node ->
      let rec incr_parse ~parse_prec ~left_pos {info; token_length; _} =
        (* The position passed into this function (left_pos) is the position that the node starts
           at, so the position of the token that triggered this node will be equal to this
           position plus the total length of the left sub-node (if there is one). For leaf, prefix
           and combinator nodes, this is just the left position. *)
        let token_start_pos = info |> Pratt.token_start_pos ~left_pos in
        let token_end_pos = token_start_pos + token_length in
        if start_pos <= token_end_pos then (* Update left or current node. *)
          (* We check if start_pos = token_end_pos in case we are updating to the very right of the
             left sub-tree. *)
          match info with
          | Infix {left; _} | Postfix {left; _} ->
            if start_pos <= token_start_pos then (* Update left. *)
              left |> incr_parse ~parse_prec ~left_pos
            else (* Update the current node. *)
              let lexer = lexer |> Lexer.move_to token_start_pos in
              {lookups; lexer; reuse} |> parse_infix ~parse_prec left
          | Leaf | Prefix _ | Combinators _ -> (* Update the current node. *)
            let lexer = lexer |> Lexer.move_to token_start_pos in
            {lookups; lexer; reuse} |> parse_prefix ~parse_prec
        else (* Update right. *)
          let tag = lookups.tag in
          let go_right ~parse_prec:curr_pp node_with_right right =
            (* Update right using parse_prec from the current node (curr_pp). *)
            let right, state = right |> incr_parse ~parse_prec:curr_pp ~left_pos:token_end_pos in
            let node = Pratt.make_node (node_with_right ~right) ~token_length ~tag in
            (* Start parsing using parse_prec from the parent node (parse_prec). *)
            state |> parse_infix ~parse_prec node
          in
          match info with
          | Prefix {parse_prec; f; right; _} ->
            right |> go_right ~parse_prec (Pratt.prefix ~parse_prec ~f)
          | Infix {parse_prec; prec; f; left; right; _} ->
            right |> go_right ~parse_prec (Pratt.infix ~parse_prec ~prec ~f ~left)
          | Combinators parse_node ->
            (* Start incrementally parsing after the token. *)
            let parse_node, lexer, reuse =
              parse_node |> update_parse change ~lexer ~reuse ~left_pos:token_end_pos in
            let info_value = (Combinators parse_node, Node.value parse_node) in
            let node = Pratt.make_node info_value ~token_length ~tag in
            (* Move the lexer to after the node and parse. *)
            let lexer = lexer |> Lexer.move_to (token_start_pos + node.total_length) in
            {lookups; lexer; reuse} |> parse_infix ~parse_prec node
          | Leaf | Postfix _ ->
            invalid_arg "Update start position is out of bounds."
      in
      let node, {lexer; reuse; _} = pratt_node |> incr_parse ~parse_prec:max_int ~left_pos in
      node, lexer, reuse

  let update ~start ~added ~removed ~lexer parse_node =
    if start < 0 || added < 0 || removed < 0 then
      invalid_arg "The arguments start, added and removed must all be non-negative."
    else if added = 0 && removed = 0 then parse_node
    else
      let change = {start_pos = start; added; removed} in
      let reuse = Reuse.create parse_node ~start_pos:start ~added ~removed in
      let parse_node, _, _ = parse_node |> update_parse change ~lexer ~reuse ~left_pos:0 in
      parse_node

  let to_ast = Node.value

  let length = Node.length
end
