module Iterator = struct
  type 'tok t = {
    rope_iter : 'tok Gadt_rope.Iterator.t;
    next : 'tok;
    pos : int;
    end_token : 'tok;
  }

  let next_or_end ~end_token rope_iter =
    match rope_iter |> Gadt_rope.Iterator.next with
    | Some next, rope_iter -> next, rope_iter
    | None, rope_iter -> end_token, rope_iter

  let make_at pos ~tokens ~end_token =
    let rope_iter = tokens |> Gadt_rope.iterator_at pos in
    let next, rope_iter = rope_iter |> next_or_end ~end_token in
    {rope_iter; next; pos; end_token}

  let make ~tokens ~end_token = make_at 0 ~tokens ~end_token

  let next {rope_iter; next; pos; end_token} =
    next,
    let next, rope_iter = rope_iter |> next_or_end ~end_token in
    {rope_iter; next; pos=pos+1; end_token}

  let peek {next; _} = next

  let pos {pos; _} = pos

  let is_at_end {next; end_token; _} = next = end_token

  let force_end ~pos ({end_token; _} as iter) = {iter with next=end_token; pos}
end

type ('tok, _) parser =
  | Eat : 'tok -> ('tok, 'tok) parser
  | Any : ('tok, 'tok) parser
  | Satisfy : ('tok -> 'a option) -> ('tok, 'a) parser
  | App : ('tok, 'a -> 'b) parser * ('tok, 'a) parser -> ('tok, 'b) parser
  | Lift : ('a -> 'b) * ('tok, 'a) parser -> ('tok, 'b) parser
  | Pratt : ('tok, 'a) lookups -> ('tok, 'a) parser
  | Fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser

and ('tok, _) parse_tree =
  | Value : 'a -> ('tok, 'a) parse_tree
  | App_node : {
      p : ('tok, 'a -> 'b) parser;
      q : ('tok, 'a) parser;
      left : ('tok, 'a -> 'b) parse_tree;
      right : ('tok, 'a) parse_tree;
      value : 'b;
      size : int;
    } -> ('tok, 'b) parse_tree
  | Lift_node : {
      f : ('a -> 'b);
      p : ('tok, 'a) parser;
      node : ('tok, 'a) parse_tree;
      value : 'b;
      size : int;
    } -> ('tok, 'b) parse_tree
  | Pratt_node : {
      lookups : ('tok, 'a) lookups;
      pratt_tree : ('tok, 'a) pratt_tree;
      value : 'a;
      size : int;
    } -> ('tok, 'a) parse_tree

and ('tok, 'a) pratt_tree =
  | Leaf of 'a
  | Prefix of {
      prec : int;
      f : 'a -> 'a;
      right : ('tok, 'a) pratt_tree;
      value : 'a;
      size : int;
    }
  | Infix of {
      prec : int;
      f : 'a -> 'a -> 'a;
      left : ('tok, 'a) pratt_tree;
      right : ('tok, 'a) pratt_tree;
      value : 'a;
      size : int;
    }
  | Postfix of {
      prec : int;
      f : 'a -> 'a;
      left : ('tok, 'a) pratt_tree;
      value : 'a;
      size : int;
    }
  | Combinators of {
      parser : ('tok, 'a) parser;
      parse_tree : ('tok, 'a) parse_tree;
      value : 'a;
      size : int;
    }

and ('tok, 'a) state = {
  lookups : ('tok, 'a) lookups;
  iter : 'tok Iterator.t;
  right_nodes : ('tok, 'a) pratt_tree list;
  right_pos : int;
}

and ('tok, 'a) lookups = {
  prefixes : ('tok -> ('tok, 'a) prefix);
  empty_prefix : ('tok, 'a) prefix;
  infixes : ('tok -> ('tok, 'a) infix);
}

and ('tok, 'a) state_f = ('tok, 'a) state -> ('tok, 'a) pratt_tree * ('tok, 'a) state

and ('tok, 'a) prefix = ('tok, 'a) state_f option

and ('tok, 'a) infix = int * (('tok, 'a) pratt_tree -> ('tok, 'a) state_f)

let value_parse = function
  | Value v -> v
  | App_node {value; _} | Lift_node {value; _} | Pratt_node {value; _} -> value

let size_parse : type a. ('tok, a) parse_tree -> int = function
  | Value _ -> 1
  | App_node {size; _} | Lift_node {size; _} | Pratt_node {size; _} -> size

let value_pratt = function
  | Leaf v -> v
  | Prefix {value; _} | Infix {value; _} | Postfix {value; _} | Combinators {value; _} -> value

let size_pratt = function
  | Leaf _ -> 1
  | Prefix {size; _} | Infix {size; _} | Postfix {size; _} | Combinators {size; _} -> size

let app_node ~p ~q left right =
  let value = value_parse left (value_parse right) in
  App_node {p; q; left; right; value; size=size_parse left + size_parse right}

let lift_node ~f ~p node =
  Lift_node {f; p; node; value=f (value_parse node); size=size_parse node}

let pratt_node ~lookups pratt_tree =
  Pratt_node {lookups; pratt_tree; value=value_pratt pratt_tree; size=size_pratt pratt_tree}

let prefix ~prec ~f right =
  Prefix {prec; f; right; value = f (value_pratt right); size = size_pratt right + 1}

let infix ~prec ~f left right =
  let value = f (value_pratt left) (value_pratt right) in
  Infix {prec; f; left; right; value; size = size_pratt left + size_pratt right + 1}

let postfix ~prec ~f left =
  Postfix {prec; f; left; value = f (value_pratt left); size = size_pratt left + 1}

let combinators ~parser ~parse_tree =
  Combinators {parser; parse_tree; value=value_parse parse_tree; size=size_parse parse_tree}

let make_state ~lookups ~iter = {lookups; iter; right_nodes=[]; right_pos=0}

let advance ({iter; right_nodes; right_pos; _} as state) =
  let _, iter = iter |> Iterator.next in
  let pos = Iterator.pos iter in
  let right_nodes, right_pos =
    match right_nodes with
    | [] -> right_nodes, right_pos
    | _::_ when pos <= right_pos -> right_nodes, right_pos
    | (Leaf _ | Postfix _ | Combinators _)::right_nodes -> right_nodes, right_pos + 1
    | (Prefix {right; _} | Infix {right; _})::right_nodes ->
      let next_right_pos = right_pos + size_pratt right + 1 in
      if next_right_pos <= pos then right_nodes, next_right_pos
      else
        let right_pos =
          match right with
          | Leaf _ | Prefix _ | Combinators _ -> right_pos + 1
          | Infix {left; _} | Postfix {left; _} -> right_pos + 1 + size_pratt left
        in
        right::right_nodes, right_pos
  in
  {state with iter; right_nodes; right_pos}

let is_at_end {iter; _} = Iterator.is_at_end iter

let lookup_prefix {lookups={prefixes; _}; iter; _} = prefixes (Iterator.peek iter)

let lookup_empty_prefix {lookups={empty_prefix; _}; _} = empty_prefix

let lookup_infix {lookups={infixes; _}; iter; _} = infixes (Iterator.peek iter)

let check_for_node ({iter; right_nodes; right_pos; _} as state) =
  let size_right = function
    | Leaf _ | Prefix _ | Combinators _ as node -> size_pratt node
    | Infix {right; _} -> size_pratt right + 1
    | Postfix _ -> 1
  in
  let pos = Iterator.pos iter in
  match right_nodes with
  | node::right_nodes when pos = right_pos ->
    let iter = iter |> Iterator.force_end ~pos:(pos + size_right node) in
    Some (node, {state with iter; right_nodes; right_pos=pos})
  | _ -> None

let log_reuse s {iter; _} =
  print_endline @@ "Reusing " ^ s ^ " at pos " ^ string_of_int (Iterator.pos iter) ^ "."

let get_prefix_node state =
  match state |> check_for_node with
  | Some ((Leaf _ | Prefix _ | Combinators _), _) as r ->
    log_reuse "leaf / prefix" state; r
  | None | Some ((Infix _ | Postfix _), _) -> None

let get_infix_node ~prec:last_prec left state =
  match state |> check_for_node with
  | Some (Infix {prec; f; right; _}, state) when prec < last_prec ->
    log_reuse "infix" state; Some (infix ~prec ~f left right, state)
  | Some (Postfix {prec; f; _}, state) when prec < last_prec ->
    log_reuse "postfix" state; Some (postfix ~prec ~f left, state)
  | _ -> None

let rec parse_infix ~prec left state =
  match state |> get_infix_node ~prec left with
  | Some (left, state) -> state |> parse_infix ~prec left
  | None ->
    if is_at_end state then left, state
    else
      let next_prec, infix = state |> lookup_infix in
      if next_prec >= prec then left, state
      else
        let node, state = state |> advance |> infix left in
        state |> parse_infix ~prec node

let parse_prefix ~prec state =
  let left, state =
    match state |> get_prefix_node with
    | Some r -> r
    | None ->
      match state |> lookup_prefix with
      | Some prefix -> state |> advance |> prefix
      | None -> match state |> lookup_empty_prefix with
        | Some prefix -> state |> prefix
        | None -> failwith ("Unexpected prefix at pos " ^ string_of_int (Iterator.pos state.iter))
  in
  state |> parse_infix ~prec left

let pratt_parse state = state |> parse_prefix ~prec:max_int

let rec run : type tok a. iter:tok Iterator.t -> (tok, a) parser ->
  (tok, a) parse_tree * tok Iterator.t = fun ~iter ->
  let fail s = failwith @@ s ^ " failed at pos " ^ (string_of_int @@ Iterator.pos iter) ^ "." in
  function
  | Eat tok ->
    let tok', iter = iter |> Iterator.next in
    if tok = tok' then Value tok, iter else fail "Eat"
  | Any ->
    let tok, iter = iter |> Iterator.next in Value tok, iter
  | Satisfy f ->
    let tok, iter = iter |> Iterator.next in begin match f tok with
      | Some v -> Value v, iter
      | None -> fail "Satisfy"
    end
  | App (p, q) ->
    let left, iter = p |> run ~iter in
    let right, iter = q |> run ~iter in
    app_node ~p ~q left right, iter
  | Lift (f, p) ->
    let node, iter = p |> run ~iter in lift_node ~f ~p node, iter
  | Pratt lookups ->
    let parse_tree, {iter; _} = make_state ~lookups ~iter |> pratt_parse in
    pratt_node ~lookups parse_tree, iter
  | Fix f ->
    f (Fix f) |> run ~iter

module Infix = struct
  let pratt_infix ~is_left prec f =
    prec,
    fun left state ->
      let prec = if is_left then prec else prec + 1 in
      let right, state = state |> parse_prefix ~prec in
      infix ~prec ~f left right, state

  let left prec f = pratt_infix ~is_left:true prec f

  let right prec f = pratt_infix ~is_left:false prec f

  let postfix ?(prec=(-2)) f =
    prec,
    fun left state -> postfix ~prec ~f left, state

  let unknown = max_int, fun _left _state -> assert false
end

module Combinators = struct
  let pratt_parser ?(empty_prefix=None) ?(infixes=fun _ -> Infix.unknown) prefixes =
    Pratt {prefixes; empty_prefix; infixes}

  let eat tok = Eat tok

  let any = Any

  let satisfy f = Satisfy f

  let (<*>) p q = App (p, q)

  let (<$>) f p = Lift (f, p)

  let ( *>) p q = (fun _ r -> r) <$> p <*> q

  let (<* ) p q = (fun r _ -> r) <$> p <*> q

  let fix f = Fix f
end

module Prefix = struct
  let some x = Some x

  let return v = some @@ fun state -> Leaf v, state

  let unary ?(prec=(-1)) f = some @@ fun state ->
    let right, state = state |> parse_prefix ~prec in
    prefix ~prec ~f right, state

  let custom parser = some @@ fun ({iter; _} as state) ->
    let parse_tree, iter = parser |> run ~iter in
    (* TODO this will unalign right nodes in state. *)
    combinators ~parser ~parse_tree, {state with iter}

  let unknown = None

  (* TODO if I allow empty infixes, make sep optional? *)
  let list parser f ~sep ~stop ~wrap =
    let open Combinators in
    let infixes = fun tok -> if tok = sep then Infix.left 1 f else Infix.unknown in
    let empty_prefix = custom parser in
    let pratt = pratt_parser ~infixes ~empty_prefix (fun _ -> unknown) in
    custom ((fun v _ -> wrap v) <$> pratt <*> eat stop)
end

module Non_incremental = struct
  let build_tree ~tokens ~end_token parser =
    let iter = Iterator.make ~tokens ~end_token in
    parser |> run ~iter |> fst

  let run ~tokens ~end_token parser = parser |> build_tree ~tokens ~end_token |> value_parse
end

module Incremental = struct
  type ('tok, 'a) t = T : {
      parser : ('tok, 'a) parser;
      end_token : 'tok;
      parse_tree : ('tok, 'a) parse_tree;
    } -> ('tok, 'a) t

  let make ~tokens ~end_token parser =
    let parse_tree = parser |> Non_incremental.build_tree ~tokens ~end_token in
    value_parse parse_tree, T {parser; end_token; parse_tree}

  (* TODO factor some of this out into a record, pass that around instead. *)
  let rec update_parse : type a. start:int -> added:int -> removed:int -> pos:int ->
    iter:'tok Iterator.t -> parser:('tok, a) parser -> ('tok, a) parse_tree ->
    ('tok, a) parse_tree * 'tok Iterator.t =
    fun ~start ~added ~removed ~pos ~iter ~parser node ->
      if start = pos && removed > size_parse node then
        (* If the changed area is greater than the size of the parsed node then reparse. *)
        (* TODO this requires more thought to get right. *)
        parser |> run ~iter
      else
        match node with
        | Value _ -> assert (start >= pos);
          parser |> run ~iter
        | Lift_node {f; p; node; _} ->
          let node, iter = node |> update_parse ~start ~added ~removed ~pos ~iter ~parser:p in
          lift_node ~f ~p node, iter
        | Pratt_node {lookups; pratt_tree; _} ->
          let pratt_tree, iter =
            update_pratt ~start ~added ~removed ~pos ~iter ~lookups pratt_tree in
          pratt_node ~lookups pratt_tree, iter
        | App_node {p; q; left; right; _} when start < pos + size_parse left -> (* Left. *)
          let left, iter = left |> update_parse ~start ~added ~removed ~pos ~iter ~parser:p in
          app_node ~p ~q left right, iter
        | App_node {p; q; left; right; _} -> (* Right. *)
          let pos = pos + size_parse left in
          let right, iter = right |> update_parse ~start ~added ~removed ~pos ~iter ~parser:q in
          app_node ~p ~q left right, iter

  and update_pratt : type a. start:int -> added:int -> removed:int -> pos:int ->
    iter:'tok Iterator.t -> lookups:('tok, a) lookups -> ('tok, a) pratt_tree ->
    ('tok, a) pratt_tree * 'tok Iterator.t =
    fun ~start ~added ~removed ~pos ~iter ~lookups pratt_tree ->
      let make_incr_state ~iter (right_nodes, right_pos) =
        {(make_state ~lookups ~iter) with right_nodes; right_pos}
      in
      let rec incr_parse ~prec ~pos:last_pos ~right_info node =
        let pos = match node with
          | Leaf _ | Prefix _ | Combinators _ -> last_pos
          | Infix {left; _} | Postfix {left; _} -> last_pos + size_pratt left
        in
        let go_right ~prec:prec' ~f make right =
          let right, state = right |> incr_parse ~prec:prec' ~pos:(pos + 1) ~right_info in
          state |> parse_infix ~prec (make ~prec ~f right)
        in
        let add_right node ((right_nodes, _) as right_info) =
          (* TODO this removes too much and doesn't consider left nodes. *)
          if pos < start + removed then right_info
          else (node::right_nodes, pos + added - removed)
        in
        match node with
        | Combinators {parser; parse_tree; _} when start > pos ->
          (* The token that triggers this has size 1, so we increment `pos`. *)
          let parse_tree, iter =
            parse_tree |> update_parse ~start ~added ~removed ~pos:(pos + 1) ~iter ~parser in
          combinators ~parser ~parse_tree, make_incr_state ~iter right_info
        | Leaf _ as left when start > pos -> (* Right. *)
          right_info |> make_incr_state ~iter |> parse_infix ~prec left
        | Prefix {prec; f; right; _} when start > pos ->
          go_right ~prec ~f prefix right
        | Infix {prec; f; left; right; _} when start > pos ->
          go_right ~prec ~f (infix left) right
        | Infix {left; _} | Postfix {left; _} when start < pos -> (* Left. *)
          left |> incr_parse ~prec ~pos:last_pos ~right_info:(right_info |> add_right node)
        | Leaf _ | Prefix _ | Infix _ | Postfix _ | Combinators _ -> (* Hit. *)
          assert (start = pos);
          let state = right_info |> add_right node |> make_incr_state ~iter in
          begin match node with
            | Leaf _ | Prefix _ | Combinators _ -> state |> parse_prefix ~prec
            | Infix {left; _} | Postfix {left; _} -> state |> parse_infix ~prec left
          end
      in
      let pratt_tree, {iter; _} = pratt_tree |> incr_parse ~pos ~prec:max_int ~right_info:([], 0) in
      pratt_tree, iter

  let update ~start ~added ~removed ~tokens (T {parser; end_token; parse_tree}) =
    let iter = Iterator.make_at start ~tokens ~end_token in
    let parse_tree, _ = parse_tree |> update_parse ~start ~added ~removed ~pos:0 ~iter ~parser in
    value_parse parse_tree, T {parser; end_token; parse_tree}
end
