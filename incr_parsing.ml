open Gadt_tree

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

module Balanced_tree = struct
  module Zipped = Zipped_trees (F_array) (F_array)

  include Append_tree (Zipped)
end

module Lexer = Incr_lexing.Lexer

type ('tok, _) parser =
  | Eat : 'tok -> ('tok, 'tok) parser
  | Any : ('tok, 'tok) parser
  | Satisfy : ('tok -> 'a option) -> ('tok, 'a) parser
  | App : ('tok, 'a -> 'b) parser * ('tok, 'a) parser -> ('tok, 'b) parser
  | Lift : ('a -> 'b) * ('tok, 'a) parser -> ('tok, 'b) parser
  | Pratt : ('tok, 'a) lookups -> ('tok, 'a) parser
  | Fix : (('tok, 'a) parser -> ('tok, 'a) parser) -> ('tok, 'a) parser

(* TODO factor out value and len? *)

and ('tok, 'a) parse_tree =
  | Basic_node : {
      value : 'a;
      len : int;
    } -> ('tok, 'a) parse_tree
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
  | Leaf of {
      value : 'a;
      len : int;
    }
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
  lexer : 'tok Lexer.t;
}

and ('tok, 'a) lookups = {
  prefixes : ('tok -> ('tok, 'a) prefix);
  empty_prefix : ('tok, 'a) prefix;
  infixes : ('tok -> ('tok, 'a) infix);
}

and ('tok, 'a) state_f = ('tok, 'a) state -> ('tok, 'a) pratt_tree * ('tok, 'a) state

and ('tok, 'a) prefix = ('tok, 'a) state_f option

and ('tok, 'a) infix = int * (('tok, 'a) pratt_tree -> ('tok, 'a) state_f)

module Node = struct
  let value = function
    | Basic_node {value; _} | App_node {value; _} | Lift_node {value; _}
    | Pratt_node {value; _} -> value

  let length : type a. ('tok, a) parse_tree -> int = function
    | Basic_node {len; _} | App_node {len; _} | Lift_node {len; _} | Pratt_node {len; _} -> len

  let basic value = Basic_node {value; len = 1}

  let app ~p ~q left right =
    let value = value left (value right) in
    App_node {p; q; left; right; value; len = length left + length right}

  let lift ~f ~p node =
    Lift_node {f; p; node; value = f (value node); len = length node}

  let pratt_value = function
    | Leaf {value; _} | Prefix {value; _} | Infix {value; _} | Postfix {value; _}
    | Combinators {value; _} -> value

  let pratt_length = function
    | Leaf {len; _} | Prefix {len; _} | Infix {len; _} | Postfix {len; _}
    | Combinators {len; _} -> len

  let pratt ~lookups pratt_tree =
    Pratt_node {lookups; pratt_tree; value = pratt_value pratt_tree; len = pratt_length pratt_tree}
end

module Pratt = struct
  let value = Node.pratt_value

  let length = Node.pratt_length

  let leaf value = Leaf {value; len = 1}

  let prefix ~prec ~f right =
    Prefix {prec; f; right; value = f (value right); len = length right + 1}

  let infix ~prec ~f left right =
    let value = f (value left) (value right) in
    Infix {prec; f; left; right; value; len = length left + length right + 1}

  let postfix ~prec ~f left =
    Postfix {prec; f; left; value = f (value left); len = length left + 1}

  let combinators ~parser ~parse_tree =
    Combinators {parser; parse_tree; value = Node.value parse_tree; len = Node.length parse_tree}
end

(*
let advance ({lexer; right_nodes; right_pos; _} as state) =
  let lexer = lexer |> Lexer.next |> snd in
  let pos = Lexer.pos lexer in
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
  {state with lexer; right_nodes; right_pos}

let check_for_node ({lexer; right_nodes; right_pos; _} as state) =
  let length_right = function
    | Leaf _ | Prefix _ | Combinators _ as node -> length_pratt node
    | Infix {right; _} -> length_pratt right + 1
    | Postfix _ -> 1
  in
  let pos = Lexer.pos lexer in
  match right_nodes with
  | node::right_nodes when pos = right_pos ->
    let lexer = lexer |> Lexer.skip (length_right node) in
    Some (node, {state with lexer; right_nodes; right_pos=pos})
  | _ -> None

let log_reuse s {lexer; _} =
  print_endline @@ "Reusing " ^ s ^ " at pos " ^ string_of_int (Lexer.pos lexer) ^ "."

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
*)

let rec parse_infix ~prec left ({lookups = {infixes; _}; lexer; _} as state) =
  let token, lexer = lexer |> Lexer.next in
  let next_prec, infix = infixes token in
  if next_prec >= prec then left, state
  else
    let node, state = infix left {state with lexer} in
    state |> parse_infix ~prec node

let parse_prefix ~prec ({lookups = {prefixes; empty_prefix; _}; lexer; _} as state) =
  let token, lexer = lexer |> Lexer.next in
  let left, state =
    match prefixes token with
    | Some prefix -> prefix {state with lexer}
    | None ->
      match empty_prefix with
      | Some prefix -> prefix state
      | None -> failwith ("Unexpected prefix at pos " ^ string_of_int (Lexer.pos state.lexer))
  in
  state |> parse_infix ~prec left

let pratt_parse state = state |> parse_prefix ~prec:max_int

let rec run : type tok a. lexer:tok Lexer.t -> (tok, a) parser ->
  (tok, a) parse_tree * tok Lexer.t = fun ~lexer parser ->
  let fail s = failwith @@ s ^ " failed at pos " ^ (string_of_int @@ Lexer.pos lexer) ^ "." in
  match parser with
  | Eat tok ->
    let tok', lexer = lexer |> Lexer.next in
    if tok = tok' then Node.basic tok, lexer else fail "Eat"
  | Any ->
    let tok, lexer = lexer |> Lexer.next in Node.basic tok, lexer
  | Satisfy f ->
    let tok, lexer = lexer |> Lexer.next in begin match f tok with
      | Some v -> Node.basic v, lexer
      | None -> fail "Satisfy"
    end
  | App (p, q) ->
    let left, lexer = p |> run ~lexer in
    let right, lexer = q |> run ~lexer in
    Node.app ~p ~q left right, lexer
  | Lift (f, p) ->
    let node, lexer = p |> run ~lexer in Node.lift ~f ~p node, lexer
  | Pratt lookups ->
    let parse_tree, {lexer; _} = pratt_parse {lookups; lexer} in
    Node.pratt ~lookups parse_tree, lexer
  | Fix f ->
    f (Fix f) |> run ~lexer

module Infix = struct
  let left prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~prec in
      Pratt.infix ~prec ~f left right, state

  let right prec f =
    prec,
    fun left state ->
      let right, state = state |> parse_prefix ~prec:(prec + 1) in
      Pratt.infix ~prec:(prec + 1) ~f left right, state

  let postfix ?(prec=(-2)) f =
    prec,
    fun left state -> Pratt.postfix ~prec ~f left, state

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

  let return v = some @@ fun state -> Pratt.leaf v, state

  let unary ?(prec=(-1)) f = some @@ fun state ->
    let right, state = state |> parse_prefix ~prec in
    Pratt.prefix ~prec ~f right, state

  let custom parser = some @@ fun ({lexer; _} as state) ->
    let parse_tree, lexer = parser |> run ~lexer in
    (* TODO need to carry across right nodes. *)
    Pratt.combinators ~parser ~parse_tree, {state with lexer}

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
  let build_tree ~lexer parser =
    let lexer = Lexer.make lexer in
    parser |> run ~lexer |> fst

  let run ~lexer parser = parser |> build_tree ~lexer |> Node.value
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
    lexer : 'tok Lexer.t
  }

  let make ~lexer parser =
    let parse_tree = parser |> Non_incremental.build_tree ~lexer in
    Node.value parse_tree, {parser; parse_tree}

  let rec update_parse : type a. 'tok update_info -> pos:int -> parser:('tok, a) parser ->
    ('tok, a) parse_tree -> ('tok, a) parse_tree * 'tok Lexer.t =
    fun ({start; added; removed; lexer} as info) ~pos ~parser -> function
      | Basic_node _ as node ->
        assert (start >= pos);
        if start > pos then node, lexer
        else parser |> run ~lexer:(Lexer.move_to pos lexer)
      | Lift_node {f; p; node; _} ->
        let node, lexer = node |> update_parse info ~pos ~parser:p in
        Node.lift ~f ~p node, lexer
      | Pratt_node {lookups; pratt_tree; _} ->
        let pratt_tree, lexer = update_pratt info ~pos ~lookups pratt_tree in
        Node.pratt ~lookups pratt_tree, lexer
      | App_node {p; q; left; right; _} ->
        let mid_pos = pos + Node.length left in
        if start > mid_pos then (* Update right only. *)
          let right, lexer = right |> update_parse info ~pos:mid_pos ~parser:q in
          Node.app ~p ~q left right, lexer
        else (* Update left and, if necessary, update right. *)
          let left, lexer = left |> update_parse info ~pos ~parser:p in
          let pos = Lexer.pos lexer in
          (* Add to right = total to add - added to left. *)
          let added = max 0 (added - pos + start) in
          (* Remove from right = total to remove - removed from left. *)
          let removed = max 0 (removed - mid_pos + start) in
          if added = 0 && removed = 0 then
            Node.app ~p ~q left right, lexer
          else (* Update right as well. *)
            let new_info = {start = pos; added; removed; lexer} in
            let right, lexer = right |> update_parse new_info ~pos ~parser:q in
            Node.app ~p ~q left right, lexer

  and update_pratt : type a. 'tok update_info -> pos:int -> lookups:('tok, a) lookups ->
    ('tok, a) pratt_tree -> ('tok, a) pratt_tree * 'tok Lexer.t =
    fun ({start; added; removed; lexer} as info) ~pos ~lookups pratt_tree ->
      let rec incr_parse ~prec ~pos:last_pos ~right_info node =
        let pos = match node with
          | Leaf _ | Prefix _ | Combinators _ -> last_pos
          | Infix {left; _} | Postfix {left; _} -> last_pos + Pratt.length left
        in
        let make_incr_state ~lexer right_info =
          let lexer = lexer |> Lexer.move_to pos in
          {lookups; lexer}
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
          (* The token that triggers this has a length, so we increase pos by this length. *)
          let parse_tree, lexer = parse_tree |> update_parse info ~pos:(pos + 1) ~parser in
          Pratt.combinators ~parser ~parse_tree, make_incr_state ~lexer right_info
        | Leaf _ as left when start > pos -> (* Right. *)
          right_info |> make_incr_state ~lexer |> parse_infix ~prec left
        | Prefix {prec; f; right; _} when start > pos ->
          go_right ~prec ~f Pratt.prefix right
        | Infix {prec; f; left; right; _} when start > pos ->
          go_right ~prec ~f (Pratt.infix left) right
        | Infix {left; _} | Postfix {left; _} when start < pos -> (* Left. *)
          left |> incr_parse ~prec ~pos:last_pos ~right_info:(right_info |> add_right node)
        | Leaf _ | Prefix _ | Infix _ | Postfix _ | Combinators _ -> (* Hit. *)
          assert (start = pos);
          let state = right_info |> add_right node |> make_incr_state ~lexer in
          begin match node with
            | Leaf _ | Prefix _ | Combinators _ -> state |> parse_prefix ~prec
            | Infix {left; _} | Postfix {left; _} -> state |> parse_infix ~prec left
          end
      in
      let pratt_tree, state = pratt_tree |> incr_parse ~pos ~prec:max_int ~right_info:([], 0) in
      pratt_tree, state.lexer

  let update ~start ~added ~removed ~lexer ({parser; parse_tree} as t) =
    let update_info = {start; added; removed; lexer = Lexer.make lexer} in
    let parse_tree, _ = parse_tree |> update_parse update_info ~pos:0 ~parser in
    Node.value parse_tree, {t with parse_tree}
end
