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

and ('tok, 'a) pratt_node =
  | Leaf of {
      value : 'a;
      token_length : int;
    }
  | Prefix of {
      f : 'a -> 'a;
      right : ('tok, 'a) pratt_node;
      parse_prec : int;
      token_length : int;
      value : 'a;
      length : int;
    }
  | Infix of {
      left : ('tok, 'a) pratt_node;
      f : 'a -> 'a -> 'a;
      right : ('tok, 'a) pratt_node;
      prec : int; (* The precedence of the operator. *)
      parse_prec : int; (* The precedence to parse at (prec + 1 for right associative infixes). *)
      token_length : int;
      value : 'a;
      length : int;
    }
  | Postfix of {
      left : ('tok, 'a) pratt_node;
      f : 'a -> 'a;
      prec : int;
      token_length : int;
      value : 'a;
      length : int;
    }
  | Combinators of {
      parse_node : ('tok, 'a) parse_node;
      token_length : int;
      value : 'a;
      length : int;
    }
  | Two_infix of {
      l : ('tok, 'a) pratt_node;
      f : 'a -> 'a -> 'a;
      r : ('tok, 'a) pratt_node;
      prec : int; (* parse_prec is the same. *)
      token_length : int;
      height : int;
      value : 'a;
      length : int;
    }
  | Three_infix of {
      l : ('tok, 'a) pratt_node;
      f : 'a -> 'a -> 'a;
      m : ('tok, 'a) pratt_node;
      g : 'a -> 'a -> 'a;
      r : ('tok, 'a) pratt_node;
      prec : int; (* parse_prec is the same. *)
      token_length_f : int;
      token_length_g : int;
      height : int;
      value : 'a;
      length : int;
    }
  (* Three_infix is treated as two two_infix nodes, of the form (l `f` (m `g` r)). Therefore the token length is taken to be token_length_f. *)

and ('tok, 'a) lookups = {
  prefixes : ('tok -> ('tok, 'a) prefix);
  empty_prefix : ('tok, 'a) prefix;
  infixes : ('tok -> ('tok, 'a) infix);
  tag : 'a Type_tag.t;
}

and ('tok, 'a) state = {
  lookups : ('tok, 'a) lookups;
  lexer : 'tok Incr_lexer.t;
  reuse : 'tok reuse_t;
}

and 'tok reuse_t = {
  min_pos : int;
  dyn_paths : 'tok dyn_path list;
}

and 'tok dyn_path =
  | Dyn_path : {
      path : (('tok, 'a) pratt_node * int) list;
      tag : 'a Type_tag.t;
    } -> 'tok dyn_path

and ('tok, 'a) state_f = token_length:int -> ('tok, 'a) state ->
  ('tok, 'a) pratt_node * ('tok, 'a) state

and ('tok, 'a) prefix =
  | Known of ('tok, 'a) state_f
  | Unknown

and ('tok, 'a) infix_without_prec =
  | Normal of (left:('tok, 'a) pratt_node -> ('tok, 'a) state_f)
  | Balancing of ('a -> 'a -> 'a)

and ('tok, 'a) infix = int * ('tok, 'a) infix_without_prec

type ('tok, 'a) parser = lexer:'tok Incr_lexer.t -> reuse:'tok reuse_t ->
  ('tok, 'a) parse_node * 'tok Incr_lexer.t * 'tok reuse_t

module Node = struct
  let pratt_value = function
    | Leaf {value; _} | Prefix {value; _} | Infix {value; _} | Postfix {value; _}
    | Combinators {value; _} | Two_infix {value; _} | Three_infix {value; _} -> value

  let pratt_length = function
    | Leaf {token_length; _} -> token_length
    | Prefix {length; _} | Infix {length; _} | Postfix {length; _} | Combinators {length; _}
    | Two_infix {length; _} | Three_infix {length; _} -> length

  let value = function
    | Satisfy {value; _} | App {value; _} | Lift {value; _} -> value
    | Pratt_parse {pratt_node; _} -> pratt_value pratt_node

  let length : type a. ('tok, a) parse_node -> int = function
    | Satisfy {length; _} | App {length; _} | Lift {length; _} -> length
    | Pratt_parse {pratt_node; _} -> pratt_length pratt_node

  let satisfy ~f ~on_error value ~length = Satisfy {f; on_error; value; length}

  let pratt ~lookups pratt_node = Pratt_parse {lookups; pratt_node}

  let lift ~f node = Lift {f; node; value = f (value node); length = length node}

  let app left right =
    let value = value left @@ value right in
    App {left; right; value; length = length left + length right}
end

module Pratt = struct
  let value = Node.pratt_value

  let length = Node.pratt_length

  let token_length = function
    | Leaf {token_length; _} -> token_length
    | Prefix {token_length; _} | Infix {token_length; _} | Postfix {token_length; _}
    | Combinators {token_length; _} | Two_infix {token_length; _} -> token_length
    | Three_infix {token_length_f; _} -> token_length_f

  let leaf value ~token_length = Leaf {value; token_length}

  let prefix ~f ~right ~parse_prec ~token_length =
    let value = f (value right) in
    let length = token_length + length right in
    Prefix {f; right; parse_prec; token_length; value; length}

  let infix ~left ~f ~right ~prec ~parse_prec ~token_length =
    let value = f (value left) (value right) in
    let length = length left + token_length + length right in
    Infix {f; left; right; prec; parse_prec; token_length; value; length}

  let postfix ~prec ~left ~f ~token_length =
    let value = f (value left) in
    let length = token_length + length left in
    Postfix {f; left; prec; token_length; value; length}

  let combinators parse_node ~token_length =
    let value = Node.value parse_node in
    let length = token_length + Node.length parse_node in
    Combinators {parse_node; token_length; value; length}

  let token_start_pos ~left_pos = function
    | Leaf _ | Prefix _ | Combinators _ -> left_pos
    | Infix {left; _} | Postfix {left; _} -> left_pos + length left
    | Two_infix {l; _} | Three_infix {l; _} -> left_pos + length l

  let two ~l ~f ~r ~prec ~token_length ~height =
    let value = f (value l) (value r) in
    let length = length l + token_length + length r in
    Two_infix {l; f; r; prec; token_length; height; value; length}

  let three ~l ~f ~m ~g ~r ~prec ~token_length_f ~token_length_g ~height =
    let value = f (value l) @@ g (value m) (value r) in
    let length = length l + token_length_f + length m + token_length_g + length r in
    Three_infix {l; f; m; g; r; prec; token_length_f; token_length_g; height; value; length}

  let three_to_two ~m ~g ~r ~prec ~token_length_g ~height =
    two ~l:m ~f:g ~r ~prec ~token_length:token_length_g ~height

  type ('tok, 'a) append_result =
    | Done of ('tok, 'a) pratt_node
    | Up of ('tok, 'a) pratt_node * ('a -> 'a -> 'a) * ('tok, 'a) pratt_node * int

  let rec append op ~left ~l_height ~right ~r_height =
    if l_height = r_height then
      let f, token_length = op in
      Up (left, f, right, token_length)
    else if l_height < r_height then (* Go left down the right node. *)
      let r_height = r_height - 1 in
      match right with
      | Two_infix {l; f; r; prec; token_length; height; _} ->
        begin match append op ~left ~l_height ~right:l ~r_height with
          | Done l -> Done (two ~l ~f ~r ~prec ~token_length ~height)
          | Up (l, f', m, token_length_f) ->
            let token_length_g = token_length in
            Done (three ~l ~f:f' ~m ~g:f ~r ~prec ~token_length_f ~token_length_g ~height)
        end
      | Three_infix {l; f; m; g; r; prec; token_length_f; token_length_g; height; _} ->
        begin match append op ~left ~l_height ~right:l ~r_height with
          | Done l -> Done (three ~l ~f ~m ~g ~r ~prec ~token_length_f ~token_length_g ~height)
          | Up (l, f', l', token_length) ->
            let left = two ~l ~f:f' ~r:l' ~prec ~token_length ~height in
            let right = two ~l:m ~f:g ~r ~prec ~token_length:token_length_g ~height in
            Up (left, f, right, token_length_f)
        end
      | _ -> assert false
    else (* if l_height > r_height *) (* Go right down the left node. *)
      let l_height = l_height - 1 in
      match left with
      | Two_infix {l; f; r; prec; token_length; height; _} ->
        begin match append op ~left:r ~l_height ~right ~r_height with
          | Done r -> Done (two ~l ~f ~r ~prec ~token_length ~height)
          | Up (m, g, r, token_length_g) ->
            let token_length_f = token_length in
            Done (three ~l ~f ~m ~g ~r ~prec ~token_length_f ~token_length_g ~height)
        end
      | Three_infix {l; f; m; g; r; prec; token_length_f; token_length_g; height; _} ->
        begin match append op ~left:r ~l_height ~right ~r_height with
          | Done r -> Done (three ~l ~f ~m ~g ~r ~prec ~token_length_f ~token_length_g ~height)
          | Up (r, f', r', token_length) ->
            let left = two ~l ~f ~r:m ~prec ~token_length:token_length_f ~height in
            let right = two ~l:r ~f:f' ~r:r' ~prec ~token_length ~height in
            Up (left, g, right, token_length_g)
        end
      | _ -> assert false

  let balanced ~left ~f ~right ~prec ~token_length =
    (* If the left node or the right node are balanced infix nodes but have a different precedence,
       they are treated as though they have height zero i.e. they are treated as leaves. *)
    let height = function
      | Leaf _ | Prefix _ | Infix _ | Postfix _ | Combinators _ -> 0
      | Two_infix {prec=prec'; height; _} | Three_infix {prec=prec'; height; _} ->
        if prec = prec' then height else 0
    in
    let l_height, r_height = height left, height right in
    match append (f, token_length) ~left ~l_height ~right ~r_height with
    | Done node -> node
    | Up (l, f, r, token_length) ->
      let height = 1 + max l_height r_height in (* Height must have increased by 1. *)
      two ~l ~f ~r ~prec ~token_length ~height

  let concat_leaves ~left nodes ~prec =
    let rec concat_same_height nodes ~height =
      let height = height + 1 in
      let rec reduce nodes acc =
        match nodes with
        | [] -> List.rev acc
        | [_] -> assert false
        | [(h, token_length_h, l); (f, token_length_f, m); (g, token_length_g, r)] ->
          let three_infix = three ~l ~f ~m ~g ~r ~prec ~token_length_f ~token_length_g ~height in
          List.rev ((h, token_length_h, three_infix) :: acc)
        | (h, token_length_h, l)::(f, token_length, r)::nodes ->
          let two_infix = two ~l ~f ~r ~prec ~token_length ~height in
          reduce nodes ((h, token_length_h, two_infix) :: acc)
      in
      match nodes with
      | [] -> assert false
      | [node] -> node
      | nodes -> reduce nodes [] |> concat_same_height ~height
    in
    let f, token_length, right = concat_same_height nodes ~height:0 in
    balanced ~left ~f ~right ~prec ~token_length
end

module Reuse : sig
  type 'tok t = 'tok reuse_t

  val empty : 'tok t

  val create : ('tok, 'a) parse_node -> start_pos:int -> added:int -> removed:int -> 'tok t

  val check_at : int -> tag:'a Type_tag.t -> 'tok t -> ('tok, 'a) pratt_node option * 'tok t
end = struct
  type 'tok t = 'tok reuse_t = {
    min_pos : int; (* This minimum position from which nodes can start being reused. *)
    dyn_paths : 'tok dyn_path list;
  }

  type ('tok, 'a) path = (('tok, 'a) pratt_node * int) list

  let empty = {min_pos = max_int; dyn_paths = []}

  let extract_pratt_nodes ~left_pos parse_node =
    let rec extract : type a. 'tok dyn_path list -> left_pos:int -> ('tok, a) parse_node ->
      'tok dyn_path list = fun dyn_paths ~left_pos -> function
      | Satisfy _ -> dyn_paths
      | Lift {node; _} -> node |> extract dyn_paths ~left_pos
      | Pratt_parse {lookups={tag; _}; pratt_node} ->
        Dyn_path {path = [(pratt_node, left_pos)]; tag} :: dyn_paths
      | App {left; right; _} ->
        let dyn_paths = right |> extract dyn_paths ~left_pos:(left_pos + Node.length left) in
        left |> extract dyn_paths ~left_pos
    in
    parse_node |> extract [] ~left_pos

  let rec seek_node ~seek_pos = function
    | [] -> []
    | (Dyn_path {path; tag})::tl as dyn_paths ->
      match path with
      | [] -> tl |> seek_node ~seek_pos
      | (_, left_pos)::_ when seek_pos < left_pos -> dyn_paths
      | (node, left_pos)::path when seek_pos >= left_pos + Pratt.length node ->
        (Dyn_path {path; tag})::tl |> seek_node ~seek_pos
      | (node, left_pos)::path ->
        down node path ~left_pos ~tag ~seek_pos ~tl

  and down : type a. ('tok, a) pratt_node -> ('tok, a) path -> left_pos:int -> tag:a Type_tag.t ->
    seek_pos:int -> tl:'tok dyn_path list -> 'tok dyn_path list =
    fun node path ~left_pos ~tag ~seek_pos ~tl ->
      let dyn_paths_push path = (Dyn_path {path; tag})::tl in
      let token_start_pos = Pratt.token_start_pos node ~left_pos in
      if token_start_pos = seek_pos then (node, left_pos)::path |> dyn_paths_push
      else if seek_pos < token_start_pos then (* Left. *)
        match node with
        | Leaf _ | Prefix _ | Combinators _ -> assert false
        | Infix {left; _} | Postfix {left; _} | Two_infix {l=left; _} | Three_infix {l=left; _} ->
          down left ((node, left_pos) :: path) ~left_pos ~tag ~seek_pos ~tl
      else (* if seek_pos > token_start_pos *)
        let token_end_pos = token_start_pos + Pratt.token_length node in
        if seek_pos < token_end_pos then dyn_paths_push path
        else match node with (* Right. *)
          | Leaf _ | Postfix _ -> assert false
          | Prefix {right; _} | Infix {right; _} | Two_infix {r=right; _} ->
            down right path ~left_pos:token_end_pos ~tag ~seek_pos ~tl
          | Three_infix {m; g; r; prec; token_length_g; height; _} ->
            let two_infix = Pratt.three_to_two ~m ~g ~r ~prec ~token_length_g ~height in
            down two_infix path ~left_pos:token_end_pos ~tag ~seek_pos ~tl
          | Combinators {parse_node; _} ->
            let dyn_paths = parse_node |> extract_pratt_nodes ~left_pos:token_end_pos in
            let dyn_paths = dyn_paths |> seek_node ~seek_pos in
            dyn_paths @ dyn_paths_push path

  let create parse_tree ~start_pos ~added ~removed =
    (* We align the original parse tree to the new parse tree by starting with an offset. *)
    let dyn_paths = parse_tree |> extract_pratt_nodes ~left_pos:(added - removed) in
    {min_pos = start_pos + added; dyn_paths}

  let check_at (type a) seek_pos ~(tag : a Type_tag.t) ({min_pos; dyn_paths} as t) :
    ('tok, a) pratt_node option * 'tok t =
    if seek_pos < min_pos then None, t
    else
      let dyn_paths = dyn_paths |> seek_node ~seek_pos in
      let t = {t with dyn_paths} in
      match dyn_paths with
      | [] | Dyn_path {path=[]; _}::_ -> None, t
      | Dyn_path {path=(node, left_pos)::_; tag=tag'}::_ ->
        if Pratt.token_start_pos node ~left_pos <> seek_pos then None, t
        else match Type_tag.compare tag tag' with
          | Type_tag.Not_equal -> None, t
          | Type_tag.Equal -> Some node, t
end

let print_reuse_info = ref false

let log_reuse s ~lexer =
  if !print_reuse_info then Printf.printf "Reusing %s at position %i.\n" s (Incr_lexer.pos lexer)

(* NOTE: Currently the end token (or closing bracket etc) will always be explicitly lexed / read,
   which isn't necessary. Need to somehow pass the end position to the parse_infix function. *)

let rec parse_infix ~parse_prec left ({lookups={infixes; tag; _}; lexer; reuse} as state) =
  let maybe_node, reuse = reuse |> Reuse.check_at (Incr_lexer.pos lexer) ~tag in
  let state = {state with reuse} in
  let reuse_and_continue node =
    log_reuse "infix" ~lexer;
    let lexer = lexer |> Incr_lexer.skip (Pratt.length node - Pratt.length left) in
    {state with lexer} |> parse_infix ~parse_prec node
  in
  match maybe_node with
  | Some (Infix {prec; _} | Postfix {prec; _} | Two_infix {prec; _} | Three_infix {prec; _})
    when prec >= parse_prec ->
    left, {state with reuse} (* The next node cannot be parsed so simply return. *)
  | Some (Infix {prec; parse_prec=orig_pp; f; right; token_length; _}) ->
    Pratt.infix ~prec ~parse_prec:orig_pp ~left ~f ~right ~token_length |> reuse_and_continue
  | Some (Postfix {prec; f; token_length; _}) ->
    Pratt.postfix ~prec ~left ~f ~token_length |> reuse_and_continue
  | Some (Two_infix {prec; f; r; token_length; _}) ->
    Pratt.balanced ~left ~f ~right:r ~prec ~token_length |> reuse_and_continue
  | Some (Three_infix {f; m; g; r; prec; token_length_f; token_length_g; height; _}) ->
    let right = Pratt.three_to_two ~m ~g ~r ~prec ~token_length_g ~height in
    Pratt.balanced ~left ~f ~right ~prec ~token_length:token_length_f |> reuse_and_continue
  | None | Some (Leaf _ | Prefix _ | Combinators _) -> (* Parsing a node. *)
    let token, token_length, lexer = lexer |> Incr_lexer.next in
    let prec, infix = infixes token in
    if prec >= parse_prec then left, state
    else
      let state = {state with lexer} in (* Advance the state, moving past the token. *)
      let node, state = match infix with
        | Normal infix -> state |> infix ~left ~token_length
        | Balancing f ->
          let right, state = state |> parse_prefix ~parse_prec:prec in
          state |> parse_balanced ~left ~prec [(f, token_length, right)]
      in
      state |> parse_infix ~parse_prec node

and parse_balanced ~left ~prec acc ({lookups={infixes; tag; _}; lexer; reuse} as state) =
  match reuse |> Reuse.check_at (Incr_lexer.pos lexer) ~tag with
  | Some _, reuse -> Pratt.concat_leaves ~left (List.rev acc) ~prec, {state with reuse}
  | None, reuse ->
    let token, token_length, lexer = lexer |> Incr_lexer.next in
    match infixes token with
    | prec', Balancing f when prec = prec' ->
      let state = {state with lexer; reuse} in
      let right, state = state |> parse_prefix ~parse_prec:prec in
      state |> parse_balanced ~left ~prec ((f, token_length, right) :: acc)
    | _ -> Pratt.concat_leaves ~left (List.rev acc) ~prec, {state with reuse}

and parse_prefix ~parse_prec ({lookups={prefixes; empty_prefix; tag; _}; lexer; reuse} as state) =
  let maybe_node, reuse = reuse |> Reuse.check_at (Incr_lexer.pos lexer) ~tag in
  let state = {state with reuse} in
  let node, state = match maybe_node with
    | Some (Leaf _ | Prefix _ | Combinators _ as node) ->
      log_reuse "prefix" ~lexer;
      let lexer = lexer |> Incr_lexer.skip (Pratt.length node) in
      node, {state with lexer}
    | None | Some (Infix _ | Postfix _ | Two_infix _ | Three_infix _) ->
      let token, token_length, lexer = lexer |> Incr_lexer.next in
      match prefixes token with
      | Known prefix -> {state with lexer} |> prefix ~token_length (* Advance the state. *)
      | Unknown ->
        match empty_prefix with
        | Known empty_prefix -> state |> empty_prefix ~token_length:0
        | Unknown ->
          failwith (Printf.sprintf "Unknown prefix at position %i." (Incr_lexer.pos state.lexer))
  in
  state |> parse_infix ~parse_prec node

module Infix = struct
  let infix ~prec f = prec, Normal f

  let left prec f = infix ~prec @@ fun ~left ~token_length state ->
    let right, state = state |> parse_prefix ~parse_prec:prec in
    Pratt.infix ~left ~f ~right ~prec ~parse_prec:prec ~token_length, state

  let right prec f = infix ~prec @@ fun ~left ~token_length state ->
    (* Using prec + 1 makes the parse tree right associative. *)
    let right, state = state |> parse_prefix ~parse_prec:(prec + 1) in
    Pratt.infix ~left ~f ~right ~prec ~parse_prec:(prec + 1) ~token_length, state

  let postfix ?(prec = -2) f = infix ~prec @@ fun ~left ~token_length state ->
    Pratt.postfix ~left ~f ~prec ~token_length, state

  (* This can't be parsed by the pratt parser as it has the lowest possible precedence. *)
  let unknown = max_int, Normal (fun ~left:_ ~token_length:_ _state -> assert false)

  let both prec f = prec, Balancing f
end

module Prefix = struct
  let known prefix = Known prefix

  let return v = known @@ fun ~token_length state ->
    Pratt.leaf v ~token_length, state

  let unary ?(prec = -1) f = known @@ fun ~token_length state ->
    let right, state = state |> parse_prefix ~parse_prec:prec in
    Pratt.prefix ~parse_prec:prec ~f ~right ~token_length, state

  let custom parser = known @@ fun ~token_length ({lexer; reuse; _} as state) ->
    let parse_node, lexer, reuse = parser ~lexer ~reuse in
    Pratt.combinators parse_node ~token_length, {state with lexer; reuse}

  let unknown = Unknown
end

let pratt_parser ?(prefixes = fun _ -> Prefix.unknown) ?(empty_prefix = Prefix.unknown)
    ?(infixes = fun _ -> Infix.unknown) ?(tag = Type_tag.fresh ()) () =
  let lookups = {prefixes; empty_prefix; infixes; tag} in
  fun ~lexer ~reuse ->
    let state = {lookups; lexer; reuse} in
    let parse_tree, {lexer; reuse; _} = state |> parse_prefix ~parse_prec:max_int in
    Node.pratt ~lookups parse_tree, lexer, reuse

module Combinators = struct
  let satisfy ?on_error f = fun ~lexer ~reuse ->
    let token, length, lexer' = lexer |> Incr_lexer.next in
    match f token with
    | Some v -> Node.satisfy ~f ~on_error v ~length, lexer', reuse (* Advance the lexer. *)
    | None ->
      match on_error with
      | Some v ->
        Printf.printf "Using error value for satisfy at position %i.\n%!" (Incr_lexer.pos lexer);
        Node.satisfy ~f ~on_error v ~length, lexer, reuse (* Do not advance the lexer. *)
      | None -> failwith @@ Printf.sprintf "Satisfy failed at position %i." (Incr_lexer.pos lexer)

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

  let non_empty_list_of p ~sep ~close =
    let rev_app_one tl = function [hd] -> hd::tl | _ -> assert false in
    let empty_prefix = Prefix.custom (p >>| fun x -> [x]) in
    let infixes token = if token = sep then Infix.left 1 rev_app_one else Infix.unknown in
    List.rev <$> pratt_parser ~empty_prefix ~infixes () <* eat close

  let list_of p ~sep ~close =
    let prefixes token = if token = close then Prefix.return [] else Prefix.unknown in
    let empty_prefix = Prefix.custom (non_empty_list_of p ~sep ~close) in
    pratt_parser ~prefixes ~empty_prefix ()

  type 'a tree = [`Leaf of 'a | `Branch of 'a tree * 'a tree]

  let non_empty_tree_of p ~sep ~close =
    let branch l r = `Branch (l, r) in
    let empty_prefix = Prefix.custom (p >>| fun x -> `Leaf x) in
    let infixes token = if token = sep then Infix.both 1 branch else Infix.unknown in
    pratt_parser ~empty_prefix ~infixes () <* eat close

  let tree_of p ~sep ~close =
    let prefixes token = if token = close then Prefix.return None else Prefix.unknown in
    let empty_prefix = Prefix.custom ((fun x -> Some x) <$> non_empty_tree_of p ~sep ~close) in
    pratt_parser ~prefixes ~empty_prefix ()
end

type change_loc = {
  start_pos : int;
  added : int;
  removed : int;
}

let rec update_parse : type a. change_loc -> lexer:'tok Incr_lexer.t -> reuse:'tok Reuse.t ->
  left_pos:int -> ('tok, a) parse_node -> ('tok, a) parse_node * 'tok Incr_lexer.t * 'tok Reuse.t =
  fun ({start_pos; added; removed} as change) ~lexer ~reuse ~left_pos -> function
    | Satisfy {f; on_error; _} ->
      Combinators.satisfy f ?on_error ~lexer:(Incr_lexer.move_to left_pos lexer) ~reuse
    | Pratt_parse {lookups; pratt_node} ->
      let node, lexer, reuse = update_pratt change ~lexer ~reuse ~left_pos ~lookups pratt_node in
      Node.pratt ~lookups node, lexer, reuse
    | Lift {f; node; _} ->
      let node, lexer, reuse = node |> update_parse change ~lexer ~reuse ~left_pos in
      Node.lift ~f node, lexer, reuse
    | App {left; right; _} ->
      let mid_pos = left_pos + Node.length left in
      if start_pos > mid_pos then (* Update right only. *)
        let right, lexer, reuse = right |> update_parse change ~lexer ~reuse ~left_pos:mid_pos in
        Node.app left right, lexer, reuse
      else (* Update left. *)
        let left, lexer, reuse = left |> update_parse change ~lexer ~reuse ~left_pos in
        let mid_pos' = left_pos + Node.length left in
        if mid_pos' = mid_pos + added - removed && mid_pos' > start_pos + added then
          (* The right node can be reused. It's position is the same once offset by
             added - removed and it is clear of all the new input (start_pos + added).
             This is a similar technique to the one used for reusing pratt nodes. *)
          Node.app left right, lexer, reuse
        else (* The right must be updated as well. *)
          let right, lexer, reuse = right |> update_parse change ~lexer ~reuse ~left_pos:mid_pos' in
          Node.app left right, lexer, reuse

and update_pratt : type a. change_loc -> lexer:'tok Incr_lexer.t -> reuse:'tok Reuse.t ->
  left_pos:int -> lookups:('tok, a) lookups -> ('tok, a) pratt_node ->
  ('tok, a) pratt_node * 'tok Incr_lexer.t * 'tok Reuse.t =
  fun ({start_pos; _} as change) ~lexer ~reuse ~left_pos ~lookups pratt_node ->
    let rec incr_parse ~parse_prec ~left_pos node =
      (* The position passed into this function (left_pos) is the position that the node starts
         at, so the position of the token that triggered this node will be equal to this
         position plus the total length of the left sub-node (if there is one). For leaf, prefix
         and combinator nodes, this is just the left position. *)
      let token_start_pos = Pratt.token_start_pos node ~left_pos in
      let token_end_pos = token_start_pos + Pratt.token_length node in
      if start_pos <= token_end_pos then (* Update left or current node. *)
        (* We check if start_pos = token_end_pos in case we are updating to the very right of the
           left sub-tree. *)
        match node with
        | Infix {left; _} | Postfix {left; _} | Two_infix {l=left; _} | Three_infix {l=left; _} ->
          if start_pos <= token_start_pos then (* Update left. *)
            left |> incr_parse ~parse_prec ~left_pos
          else (* Update the current node. *)
            let lexer = lexer |> Incr_lexer.move_to token_start_pos in
            {lookups; lexer; reuse} |> parse_infix ~parse_prec left
        | Leaf _ | Prefix _ | Combinators _ -> (* Update the current node. *)
          let lexer = lexer |> Incr_lexer.move_to token_start_pos in
          {lookups; lexer; reuse} |> parse_prefix ~parse_prec
      else (* Update right. *)
        let go_right ~parse_prec:curr_pp node_with_right right =
          (* Update right using parse_prec from the current node (curr_pp). *)
          let right, state = right |> incr_parse ~parse_prec:curr_pp ~left_pos:token_end_pos in
          (* Start parsing using parse_prec from the parent node (parse_prec). *)
          state |> parse_infix ~parse_prec (node_with_right ~right)
        in
        match node with
        | Prefix {parse_prec; f; right; token_length; _} ->
          right |> go_right ~parse_prec (Pratt.prefix ~parse_prec ~f ~token_length)
        | Infix {parse_prec; prec; f; left; right; token_length; _} ->
          right |> go_right ~parse_prec (Pratt.infix ~parse_prec ~prec ~left ~f ~token_length)
        | Two_infix {l; f; r; prec; token_length; _} ->
          r |> go_right ~parse_prec:prec (Pratt.balanced ~prec ~left:l ~f ~token_length)
        | Three_infix {l; f; m; g; r; prec; token_length_f; token_length_g; height; _} ->
          let two_infix = Pratt.three_to_two ~m ~g ~r ~prec ~token_length_g ~height in
          let make = Pratt.balanced ~prec ~left:l ~f ~token_length:token_length_f in
          two_infix |> go_right ~parse_prec:prec make
        | Combinators {parse_node; token_length; _} ->
          (* Start incrementally parsing after the token. *)
          let parse_node, lexer, reuse =
            parse_node |> update_parse change ~lexer ~reuse ~left_pos:token_end_pos in
          let node = Pratt.combinators parse_node ~token_length in
          (* Move the lexer to after the node and parse. *)
          let lexer = lexer |> Incr_lexer.move_to (token_start_pos + Pratt.length node) in
          {lookups; lexer; reuse} |> parse_infix ~parse_prec node
        | Leaf _ | Postfix _ ->
          invalid_arg "Update start position is out of bounds."
    in
    let node, {lexer; reuse; _} = pratt_node |> incr_parse ~parse_prec:max_int ~left_pos in
    node, lexer, reuse

module Parse_tree = struct
  type ('tok, 'a) t = ('tok, 'a) parse_node

  let with_tag_check f =
    let count = Type_tag.tag_count () in
    let result = f () in
    if count <> Type_tag.tag_count () then
      prerr_endline "Warning: type tags should not be generated during parsing.";
    result

  let create parser ~lexer = with_tag_check @@ fun () ->
    let lexer = lexer |> Incr_lexer.move_to 0 in
    let t, _, _ = parser ~lexer ~reuse:Reuse.empty in t

  let update ~start ~added ~removed ~lexer t =
    if start < 0 || added < 0 || removed < 0 then
      invalid_arg "The arguments start, added and removed must all be non-negative."
    else if added = 0 && removed = 0 then t
    else
      let change = {start_pos = start; added; removed} in
      let reuse = Reuse.create t ~start_pos:start ~added ~removed in
      with_tag_check @@ fun () ->
      let t, _, _ = t |> update_parse change ~lexer ~reuse ~left_pos:0 in t

  let value = Node.value

  let length = Node.length
end
