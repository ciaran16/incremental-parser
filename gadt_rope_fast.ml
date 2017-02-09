module type Container = sig
  type 'a t

  val max_leaf_size : int

  val length : 'a t -> int

  val get : 'a t -> int -> 'a

  val append : 'a t -> 'a t -> 'a t

  val sub : 'a t -> int -> int -> 'a t
end

module type S = sig
  type 'a t

  val length : 'a t -> int

  val append : 'a t -> 'a t -> 'a t

  val split_exn : int -> 'a t -> 'a t * 'a t

  module Fast_iterator : sig
    type 'a it

    val start_at : int -> 'a t -> 'a it option

    val is_at_end : 'a it -> bool

    val next : 'a it -> 'a * 'a it

    val skip : int -> 'a it -> 'a it
  end
end

module Make (C : Container) : sig
  include S

  val of_container : 'a C.t -> 'a t
end = struct
  type z = Z

  type 'n s = S

  type ('a, _) tree =
    | Leaf : 'a C.t -> ('a, z) tree
    | Two : ('a, 'n) tree * ('a, 'n) tree * int -> ('a, 'n s) tree
    | Three : ('a, 'n) tree * ('a, 'n) tree * ('a, 'n) tree * int -> ('a, 'n s) tree

  type 'a t =
    | Flat : 'a C.t -> 'a t
    | Tree : ('a, 'n) tree -> 'a t

  let max_leaf_size = C.max_leaf_size

  let min_leaf_size = (max_leaf_size + 1) / 2

  let (^^) = C.append

  let halve c =
    let len = C.length c in let len2 = len / 2 in
    C.sub c 0 len2, C.sub c len2 (len - len2)

  let split_container c i =
    C.sub c 0 i, C.sub c i (C.length c - i)

  let size : type n. ('a, n) tree -> int = function
    | Leaf c -> C.length c
    | Two (_, _, size) -> size
    | Three (_, _, _, size) -> size

  let length = function
    | Flat c -> C.length c
    | Tree tree -> size tree

  let flat c =
    assert (C.length c < min_leaf_size);
    Flat c

  let leaf c =
    let len = C.length c in assert (len <= max_leaf_size && len >= min_leaf_size);
    Leaf c

  let leaf_or_flat c = if C.length c < min_leaf_size then flat c else Tree (leaf c)

  let two l r = Two (l, r, size l + size r)

  let three l m r = Three (l, m, r, size l + size m + size r)

  let of_container (c : 'a C.t) : 'a t =
    let len = C.length c in
    if len <= max_leaf_size then leaf_or_flat c
    else
      let rec reduce1 : type n. ('a, n s) tree list -> ('a, n) tree list ->
        ('a, n s) tree list = fun acc -> function
        | [] -> List.rev acc
        | [_] -> assert false
        | [l; m; r] -> List.rev (three l m r :: acc)
        | l::r::nodes -> reduce1 (two l r :: acc) nodes
      in
      let rec reduce : type n. ('a, n) tree list -> 'a t = function
        | [] -> assert false
        | [tree] -> Tree tree
        | nodes -> nodes |> reduce1 [] |> reduce
      in
      let rec list_leaves i acc =
        if i = 0 then acc
        else if i < max_leaf_size * 2 then
          (* c1 and c2 will both be at least min_leaf_size in length as we only decrease i
             by max_leaf_size when recursing. *)
          let c1, c2 = halve c in leaf c1 :: leaf c2 :: acc
        else
          let i' = i - max_leaf_size in list_leaves i' (leaf (C.sub c i' max_leaf_size) :: acc)
      in
      list_leaves len [] |> reduce

  let out_of_bounds = Invalid_argument "Index out of bounds."

  type ('a, 'n) append_result =
    | Done of ('a, 'n) tree
    | Up of ('a, 'n) tree * ('a, 'n) tree

  type ('a, _) one_tree =
    | One : ('a, 'n) one_tree -> ('a, 'n s) one_tree
    | Wrap : ('a, 'n) tree -> ('a, 'n) one_tree
    | Short : 'a C.t -> ('a, z) one_tree

  (* Concats two containers, assuming the combined length is greater than min_leaf_size. *)
  let append_containers c1 c2 =
    let l1 = C.length c1 in let len = l1 + C.length c2 in
    assert (len >= min_leaf_size);
    if len <= max_leaf_size then Done (leaf (c1 ^^ c2))
    else
      let m = len / 2 in
      let c1, c2 =
        if l1 < m then let c2', c2'' = split_container c2 (m - l1) in c1 ^^ c2', c2''
        else if l1 > m then let c1', c1'' = split_container c1 m in c1', c1'' ^^ c2
        else c1, c2
      in Up (leaf c1, leaf c2)

  let rec append_right : type n. ('a, n) tree -> ('a, n) one_tree ->
    ('a, n) append_result = fun tree one_tree -> match tree, one_tree with
    | Leaf c1, Short c2 -> append_containers c1 c2
    | l, Wrap r -> Up (l, r)
    | Two (l, r, _), One r' ->
      begin match append_right r r' with
        | Done r -> Done (two l r)
        | Up (m, r) -> Done (three l m r)
      end
    | Three (l, m, r, _), One r' ->
      begin match append_right r r' with
        | Done r -> Done (three l m r)
        | Up (r, r') -> Up (two l m, two r r')
      end

  let rec append_left : type n. ('a, n) one_tree -> ('a, n) tree ->
    ('a, n) append_result = fun tree one_tree -> match tree, one_tree with
    | Short c1, Leaf c2 -> append_containers c1 c2
    | Wrap l, r -> Up (l, r)
    | One l', Two (l, r, _) ->
      begin match append_left l' l with
        | Done l -> Done (two l r)
        | Up (l, m) -> Done (three l m r)
      end
    | One l', Three (l, m, r, _) ->
      begin match append_left l' l with
        | Done l -> Done (three l m r)
        | Up (l', l) -> Up (two l' l, two m r)
      end

  type ('a, _) down_tree =
    | Start : ('a, 'n) tree -> ('a, 'n) down_tree
    | Down : ('a, 'n s) down_tree -> ('a, 'n) down_tree

  type 'a balanced_pair =
    | Balanced : ('a, 'n) tree * ('a, 'n) tree -> 'a balanced_pair
    | Left : ('a, 'n) one_tree * ('a, 'n) tree -> 'a balanced_pair
    | Right : ('a, 'n) tree * ('a, 'n) one_tree -> 'a balanced_pair

  let append t1 t2 =
    let down tree =
      let rec down : type n. ('a, n) down_tree -> ('a, n) tree -> ('a, z) down_tree = fun d -> function
        | Leaf _ -> d
        | Two (sub, _, _) -> down (Down d) sub
        | Three (sub, _, _, _) -> down (Down d) sub
      in
      down (Start tree) tree
    in
    let rec balance_left : type n. ('a, n) one_tree -> ('a, n) down_tree ->
      'a balanced_pair = fun l -> function
      | Start r -> Left (l, r)
      | Down r -> balance_left (One l) r
    in
    let rec balance_right : type n. ('a, n) down_tree -> ('a, n) one_tree ->
      'a balanced_pair = fun l r -> match l with
      | Start l -> Right (l, r)
      | Down l -> balance_right l (One r)
    in
    let rec make_balanced_pair : type n. ('a, n) down_tree -> ('a, n) down_tree ->
      'a balanced_pair = fun l r -> match l, r with
      | Start l, Start r -> Balanced (l, r)
      | Down l, Down r -> make_balanced_pair l r
      | Down l, Start r -> balance_right l (One (Wrap r))
      | Start l, Down r -> balance_left (One (Wrap l)) r
    in
    let convert_append_result = function
      | Done tree -> Tree tree
      | Up (l, r) -> Tree (two l r)
    in
    let append_balanced_pair = function
      | Balanced (l, r) -> Tree (two l r)
      | Left (l, r) -> append_left l r |> convert_append_result
      | Right (l, r) -> append_right l r |> convert_append_result
    in
    match t1, t2 with
    | Flat c1, Flat c2 -> leaf_or_flat (c1 ^^ c2)
    | Flat c, Tree tree -> balance_left (Short c) (down tree) |> append_balanced_pair
    | Tree tree, Flat c -> balance_right (down tree) (Short c) |> append_balanced_pair
    | Tree l, Tree r -> make_balanced_pair (down l) (down r) |> append_balanced_pair

  let split_exn i = function
    | Flat c -> let c1, c2 = split_container c i in flat c1, flat c2
    | Tree tree ->
      let rec split : type n. int -> ('a, n) tree -> ('a, n) one_tree * ('a, n) one_tree = fun i node ->
        let convert = function
          | Done node -> One (Wrap node)
          | Up (l, r) -> Wrap (two l r)
        in
        let short_or_leaf c = if C.length c < min_leaf_size then Short c else Wrap (leaf c) in
        match node with
        | Leaf c ->
          if i >= C.length c then raise out_of_bounds
          else let c1, c2 = split_container c i in short_or_leaf c1, short_or_leaf c2
        | Two (l, r, _) ->
          let size_l = size l in
          if i < size_l then let l, l' = l |> split i in One l, convert (append_left l' r)
          else if i = size l then One (Wrap l), One (Wrap r)
          else let r, r' = r |> split (i - size_l) in convert (append_right l r), One r'
        | Three (l, m, r, _) ->
          let size_l = size l in
          let size_l_m = size_l + size m in
          if i < size_l then
            let l, l' = l |> split i in
            let r = match append_left l' m with
              | Done l -> two l r
              | Up (l, m) -> three l m r
            in
            One l, Wrap r
          else if i = size_l then One (Wrap l), Wrap (two m r)
          else if i < size_l_m then
            let m, m' = m |> split (i - size_l) in
            convert (append_right l m), convert (append_left m' r)
          else if i = size_l_m then Wrap (two l m), One (Wrap r)
          else
            let r, r' = r |> split (i - size_l_m) in
            let l = match append_right m r with
              | Done r -> two l r
              | Up (m, r) -> three l m r
            in
            Wrap l, One r'
      in
      let rec convert_one_tree : type n. ('a, n) one_tree -> 'a t = function
        | Wrap tree -> Tree tree
        | One o -> convert_one_tree o
        | Short c -> flat c
      in
      let l, r = tree |> split i in convert_one_tree l, convert_one_tree r

  module Fast_iterator = struct
    type 'a it = {
      path_o : ('a, z) path option;
      c : 'a C.t;
      i : int;
      ended : bool;
    }

    and ('a, _) path =
      | Top : ('a, 'n s) tree -> ('a, 'n s) path
      | Node : {
          path : ('a, 'n s) path;
          node : ('a, 'n) tree;
          offset : int;
        } -> ('a, 'n) path

    let rec get : type n. int -> ('a, n) path -> 'a it = fun i path ->
      let down (type n) (node : ('a, n s) tree) ~path ~offset =
        let node, i' = match node with
          | Two (t1, t2, _) ->
            let n1 = size t1 in
            if i < n1 then (t1, i)
            else (t2, i - n1)
          | Three (t1, t2, t3, _) ->
            let n1 = size t1 in
            let n2 = n1 + size t2 in
            if i < n1 then (t1, i)
            else if i < n2 then (t2, i - n1)
            else (t3, i - n2)
        in
        get i' (Node {path; node; offset = offset + i - i'})
      in
      match path with
      | Node {node=Leaf c; _} -> {path_o=Some path; c; i; ended = i >= C.length c}
      | Top (Two _ as node) -> down node ~path ~offset:0
      | Top (Three _ as node) -> down node ~path ~offset:0
      | Node {node=Two _ as node; offset; _} -> down node ~path ~offset
      | Node {node=Three _ as node; offset; _} -> down node ~path ~offset

    let move : type n. int -> ('a, n) path -> 'a it = fun i -> function
      | Top _ as path -> path |> get i
      | Node {path; offset; _} ->
        let rec move : type n. int -> ('a, n s) path -> 'a it = fun pos -> function
          | Top _ as path -> get pos path
          | Node {path=up_path; node; offset} as path ->
            let i = pos - offset in
            if i >= 0 && i < size node then get i path
            else move pos up_path
        in
        path |> move (offset + i)

    let start_at pos = function
      | Flat c -> if C.length c = 0 then None else Some ({path_o=None; c; i=pos; ended=false})
      | Tree (Leaf c) -> Some ({path_o=None; c; i=pos; ended=false})
      | Tree (Two _ as tree) -> Some ((Top tree) |> get pos)
      | Tree (Three _ as tree) -> Some ((Top tree) |> get pos)

    let is_at_end {ended; _} = ended

    let next ({path_o; c; i; ended} as t) =
      let len = C.length c in
      if i < len then C.get c i, {t with i = i + 1}
      else if ended then C.get c (len - 1), t
      else match path_o with
        | None -> C.get c (len - 1), {t with ended=true}
        | Some path -> let ({c; i; _} as t) = path |> move (i + 1) in C.get c i, t

    let skip n ({i; _} as t) = {t with i = i + n}
  end
end

module Functional_array = struct
  module Array_container = struct
    type 'a t = 'a array

    let max_leaf_size = 256

    let length = Array.length

    let get = Array.get

    let append = Array.append

    let sub = Array.sub
  end

  include Make (Array_container)

  let of_array = of_container
end

module String_rope = struct
  module String_container = struct
    type 'a t = Str : string -> char t

    let max_leaf_size = 1024

    let length : type a. a t -> int = fun (Str s) -> String.length s

    let get : type a. a t -> int -> a = fun (Str s) i -> String.get s i

    let sub : type a. a t -> int -> int -> a t = fun (Str s) a b -> Str (String.sub s a b)

    let append : type a. a t -> a t -> a t = fun (Str s1) (Str s2) -> Str (s1 ^ s2)
  end

  include Make (String_container)

  let of_string s = of_container (String_container.Str s)
end

module One_rope = struct
  module One_container : Container = struct
    type 'a t = 'a

    let max_leaf_size = 1

    let length _ = 1

    let get t i = assert (i = 0); t

    let append _ _ = assert false

    let sub t a b = assert (a = 0 && b = 1); t
  end

  include Make (One_container)

  (* TODO no way to actually create. *)
end
