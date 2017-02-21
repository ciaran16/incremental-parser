open Gadt_tree

module Balanced_tree = struct
  module Zipped = Zipped_trees (F_array) (F_array)

  include Append_tree (Zipped)
end

module Iter_wrapper = struct
  module Iterator = F_array.Iterator

  type 'tok t = {
    iter : 'tok Iterator.t;
    pos : int;
    peek : ('tok * 'tok Iterator.t) Lazy.t;
  }

  let make iter pos = {iter; pos; peek = lazy (Iterator.next iter)}

  let start_at pos tokens =
    match tokens |> Iterator.start_at pos with
    | None -> failwith "No tokens given to parser - there should be at least an end token."
    | Some iter -> make iter pos

  let next {pos; peek; _} =
    let next, iter = Lazy.force peek in
    next, make iter (pos + 1)

  let peek {peek; _} = Lazy.force peek |> fst

  let skip n {iter; pos; _} = make (iter |> Iterator.skip n) (pos + n)

  let pos {pos; _} = pos
end

module Type = struct
  type (_, _) is_equal =
    | Equal : ('a, 'a) is_equal
    | Not_equal : ('a, 'b) is_equal

  let equal = Equal

  let not_equal = Not_equal
end

type ('tok, _) parser =
  | Eat : 'tok -> ('tok, 'tok) parser
  | Any : ('tok, 'tok) parser
  | Satisfy : ('tok -> 'a option) -> ('tok, 'a) parser
  | App : ('tok, 'a -> 'b) parser * ('tok, 'a) parser -> ('tok, 'b) parser
  | Lift : ('a -> 'b) * ('tok, 'a) parser -> ('tok, 'b) parser
  | Pratt : ('tok, 'a) lookups -> ('tok, 'a) parser
  | Fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser

and ('tok, 'a) parse_tree =
  | Value : 'a -> ('tok, 'a) parse_tree
  | App_node : {
      p : ('tok, 'a -> 'b) parser;
      q : ('tok, 'a) parser;
      left : ('tok, 'a -> 'b) parse_tree;
      right : ('tok, 'a) parse_tree;
      value : 'b;
      len : int;
    } -> ('tok, 'b) parse_tree
  | Lift_node : {
      f : ('a -> 'b);
      p : ('tok, 'a) parser;
      node : ('tok, 'a) parse_tree;
      value : 'b;
      len : int;
    } -> ('tok, 'b) parse_tree
  | Pratt_node : {
      lookups : ('tok, 'a) lookups;
      pratt_tree : ('tok, 'a) pratt_tree;
      value : 'a;
      len : int;
    } -> ('tok, 'a) parse_tree

and ('tok, 'a) pratt_tree =
  | Leaf of 'a
  | Prefix of {
      prec : int;
      f : 'a -> 'a;
      right : ('tok, 'a) pratt_tree;
      value : 'a;
      len : int;
    }
  | Infix of {
      prec : int;
      f : 'a -> 'a -> 'a;
      left : ('tok, 'a) pratt_tree;
      right : ('tok, 'a) pratt_tree;
      value : 'a;
      len : int;
    }
  | Postfix of {
      prec : int;
      f : 'a -> 'a;
      left : ('tok, 'a) pratt_tree;
      value : 'a;
      len : int;
    }
  | Combinators of {
      parser : ('tok, 'a) parser;
      parse_tree : ('tok, 'a) parse_tree;
      value : 'a;
      len : int;
    }
  (*| Balanced : {
      prec : int;
      f : 'b F_array.t -> 'a;
      tree : (('tok, 'a) pratt_tree * 'b) Balanced_tree.t;
      len : int;
    } -> ('tok, 'a) pratt_tree*)

and ('tok, 'a) state = {
  lookups : ('tok, 'a) lookups;
  iter : 'tok Iter_wrapper.t;
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

let length_parse : type a. ('tok, a) parse_tree -> int = function
  | Value _ -> 1
  | App_node {len; _} | Lift_node {len; _} | Pratt_node {len; _} -> len

let value_pratt = function
  | Leaf v -> v
  | Prefix {value; _} | Infix {value; _} | Postfix {value; _} | Combinators {value; _} -> value

let length_pratt = function
  | Leaf _ -> 1
  | Prefix {len; _} | Infix {len; _} | Postfix {len; _} | Combinators {len; _} -> len

let app_node ~p ~q left right =
  let value = value_parse left (value_parse right) in
  App_node {p; q; left; right; value; len = length_parse left + length_parse right}

let lift_node ~f ~p node =
  Lift_node {f; p; node; value = f (value_parse node); len = length_parse node}

let pratt_node ~lookups pratt_tree =
  Pratt_node {lookups; pratt_tree; value = value_pratt pratt_tree; len = length_pratt pratt_tree}

let prefix ~prec ~f right =
  Prefix {prec; f; right; value = f (value_pratt right); len = length_pratt right + 1}

let infix ~prec ~f left right =
  let value = f (value_pratt left) (value_pratt right) in
  Infix {prec; f; left; right; value; len = length_pratt left + length_pratt right + 1}

let postfix ~prec ~f left =
  Postfix {prec; f; left; value = f (value_pratt left); len = length_pratt left + 1}

let combinators ~parser ~parse_tree =
  Combinators {parser; parse_tree; value = value_parse parse_tree; len = length_parse parse_tree}

let make_state ~lookups ~iter = {lookups; iter; right_nodes=[]; right_pos=0}

let get_iterator {iter; _} = iter

let advance ({iter; right_nodes; right_pos; _} as state) =
  let iter = iter |> Iter_wrapper.next |> snd in
  let pos = Iter_wrapper.pos iter in
  let right_nodes, right_pos =
    match right_nodes with
    | [] -> right_nodes, right_pos
    | _::_ when pos <= right_pos -> right_nodes, right_pos
    | (Leaf _ | Postfix _ | Combinators _)::right_nodes -> right_nodes, right_pos + 1
    | (Prefix {right; _} | Infix {right; _})::right_nodes ->
      let next_right_pos = right_pos + length_pratt right + 1 in
      if next_right_pos <= pos then right_nodes, next_right_pos
      else
        let right_pos =
          match right with
          | Leaf _ | Prefix _ | Combinators _ -> right_pos + 1
          | Infix {left; _} | Postfix {left; _} -> right_pos + 1 + length_pratt left
        in
        right::right_nodes, right_pos
  in
  {state with iter; right_nodes; right_pos}

let lookup_prefix {lookups={prefixes; _}; iter; _} = prefixes (Iter_wrapper.peek iter)

let lookup_empty_prefix {lookups={empty_prefix; _}; _} = empty_prefix

let lookup_infix {lookups={infixes; _}; iter; _} = infixes (Iter_wrapper.peek iter)

let check_for_node ({iter; right_nodes; right_pos; _} as state) =
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
  | _ -> None

let log_reuse s {iter; _} =
  print_endline @@ "Reusing " ^ s ^ " at pos " ^ string_of_int (Iter_wrapper.pos iter) ^ "."

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
      | None ->
        match state |> lookup_empty_prefix with
        | Some prefix -> state |> prefix
        | None ->
          failwith ("Unexpected prefix at pos " ^ string_of_int (Iter_wrapper.pos state.iter))
  in
  state |> parse_infix ~prec left

let pratt_parse state = state |> parse_prefix ~prec:max_int

let rec run : type tok a. iter:tok Iter_wrapper.t -> (tok, a) parser ->
  (tok, a) parse_tree * tok Iter_wrapper.t = fun ~iter ->
  let fail s = failwith @@ s ^ " failed at pos " ^ (string_of_int @@ Iter_wrapper.pos iter) ^ "." in
  function
  | Eat tok ->
    let tok', iter = iter |> Iter_wrapper.next in
    if tok = tok' then Value tok, iter else fail "Eat"
  | Any ->
    let tok, iter = iter |> Iter_wrapper.next in Value tok, iter
  | Satisfy f ->
    let tok, iter = iter |> Iter_wrapper.next in begin match f tok with
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
  let left prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~prec in
      infix ~prec ~f left right, state

  let right prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~prec:(prec + 1) in
      infix ~prec:(prec + 1) ~f left right, state

  let postfix ?(prec=(-2)) f =
    prec,
    fun left state -> postfix ~prec ~f left, state

  (*let balanced prec =
    prec,
    fun left state ->
      (* TODO check if this makes it not use up stack space. However may be slower.
         Could possibly do a combination of the two? *)
      let prec = prec + 1 in
      let right, state = state |> parse_prefix ~prec in
      balanced ~prec left right, state*)

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

  let custom parser = some @@ fun ({lookups; _} as state) ->
    let iter = state |> get_iterator in
    let parse_tree, iter = parser |> run ~iter in
    (* TODO need to carry across right nodes. *)
    combinators ~parser ~parse_tree, make_state ~lookups ~iter

  let unknown = None

  (* TODO Option to handle trailing comma?
  let list p ~sep ~close f =
    let nil = Balanced_tree.empty value_parse in
    let ps = fix @@ fun ps ->
      let prefixes tok =
        if tok = sep then custom (Balanced_tree.cons <$> p <*> ps)
        else if tok = close then return nil
        else unknown
      in
      pratt_parser prefixes
    in
    let start =
      let prefixes tok = if tok = close then return nil else unknown in
      let empty_prefix = custom (Balanced_tree.cons <$> p <*> ps) in
      pratt_parser prefixes ~empty_prefix
    in
     custom (f <$> start) *)
end

module Non_incremental = struct
  let build_tree ~tokens parser =
    let iter = Iter_wrapper.start_at 0 tokens in
    parser |> run ~iter |> fst

  let run ~tokens parser = parser |> build_tree ~tokens |> value_parse
end

module Incremental = struct
  type ('tok, 'a) t = {
    parser : ('tok, 'a) parser;
    parse_tree : ('tok, 'a) parse_tree;
  }

  type 'tok update_info = {
    start : int;
    added : int;
    removed : int;
    iter : 'tok Iter_wrapper.t
  }

  let make ~tokens parser =
    let parse_tree = parser |> Non_incremental.build_tree ~tokens in
    value_parse parse_tree, {parser; parse_tree}

  let rec update_parse : type a. 'tok update_info -> pos:int -> parser:('tok, a) parser ->
    ('tok, a) parse_tree -> ('tok, a) parse_tree * 'tok Iter_wrapper.t =
    fun ({start; added; removed; iter} as info) ~pos ~parser -> function
      | Value _ as node ->
        if start > pos then node, iter else parser |> run ~iter
      | Lift_node {f; p; node; _} ->
        let node, iter = node |> update_parse info ~pos ~parser:p in
        lift_node ~f ~p node, iter
      | Pratt_node {lookups; pratt_tree; _} ->
        let pratt_tree, iter = update_pratt info ~pos ~lookups pratt_tree in
        pratt_node ~lookups pratt_tree, iter
      | App_node {p; q; left; right; _} ->
        let mid_pos = pos + length_parse left in
        if start > mid_pos then (* Update right only. *)
          let right, iter = right |> update_parse info ~pos:mid_pos ~parser:q in
          app_node ~p ~q left right, iter
        else (* Update left and if necessary update right. *)
          let left, iter = left |> update_parse info ~pos ~parser:p in
          let pos = Iter_wrapper.pos iter in
          (* Add to right = total to add - added to left. *)
          let added = max 0 (added - pos + start) in
          (* Remove from right = total to remove - removed from left. *)
          let removed = max 0 (removed - mid_pos + start) in
          if added = 0 && removed = 0 then (* Updated left only. *)
            app_node ~p ~q left right, iter
          else (* Update right as well. *)
            let new_info = {start=pos; added; removed; iter} in
            let right, iter = right |> update_parse new_info ~pos ~parser:q in
            app_node ~p ~q left right, iter

  and update_pratt : type a. 'tok update_info -> pos:int -> lookups:('tok, a) lookups ->
    ('tok, a) pratt_tree -> ('tok, a) pratt_tree * 'tok Iter_wrapper.t =
    fun ({start; added; removed; iter} as info) ~pos ~lookups pratt_tree ->
      let make_incr_state ~iter (right_nodes, right_pos) =
        {(make_state ~lookups ~iter) with right_nodes; right_pos}
      in
      let rec incr_parse ~prec ~pos:last_pos ~right_info node =
        let pos = match node with
          | Leaf _ | Prefix _ | Combinators _ -> last_pos
          | Infix {left; _} | Postfix {left; _} -> last_pos + length_pratt left
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
          (* The token that triggers this has a length, so we increase pos by length. *)
          let parse_tree, iter = parse_tree |> update_parse info ~pos:(pos + 1) ~parser in
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
      let pratt_tree, state = pratt_tree |> incr_parse ~pos ~prec:max_int ~right_info:([], 0) in
      pratt_tree, state |> get_iterator

  let update ~start ~added ~removed ~tokens {parser; parse_tree} =
    let update_info = {start; added; removed; iter = Iter_wrapper.start_at start tokens} in
    let parse_tree, _ = parse_tree |> update_parse update_info ~pos:0 ~parser in
    value_parse parse_tree, {parser; parse_tree}
end
