exception Out_of_bounds of string

let extract_exn s = function
  | None -> raise (Out_of_bounds s)
  | Some x -> x

module List = struct
  include List

  (* This makes map tail recursive, in order to avoid stack overflows.
     The functions that use it are unlikely to be working on very long lists but just in case. *)
  let map f l = l |> List.rev |> List.rev_map f
end

module type S = sig
  type 'a t

  type 'a tree = 'a t

  val empty : 'a t

  val length : 'a t -> int

  val get : int -> 'a t -> 'a option

  val get_exn : int -> 'a t -> 'a

  val append : l:'a t -> r:'a t -> 'a t

  val concat : 'a t list -> 'a t

  val split : int -> 'a t -> ('a t * 'a t) option

  val split_exn : int -> 'a t -> 'a t * 'a t

  val sub : int -> int -> 'a t -> 'a t option

  val sub_exn : int -> int -> 'a t -> 'a t

  module Iterator : sig
    type 'a t

    val start_at : int -> 'a tree -> 'a t option

    val is_at_end : 'a t -> bool

    val next : 'a t -> 'a * 'a t

    val skip : int -> 'a t -> 'a t
  end
end

module type Container = sig
  type 'a t

  val max_leaf_size : int

  val length : 'a t -> int

  val get : 'a t -> int -> 'a

  val append : 'a t -> 'a t -> 'a t

  val sub : 'a t -> int -> int -> 'a t
end

module Make (C : Container) = struct
  type z = Z

  type 'n s = S

  type ('a, _) node =
    | Leaf : 'a C.t -> ('a, z) node
    | Two : ('a, 'n) node * ('a, 'n) node * int -> ('a, 'n s) node
    | Three : ('a, 'n) node * ('a, 'n) node * ('a, 'n) node * int -> ('a, 'n s) node

  type 'a t =
    | Empty : 'a t
    | Flat : 'a C.t -> 'a t
    | Node : ('a, 'n) node -> 'a t

  type 'a tree = 'a t

  let empty = Empty

  let max_leaf_size =
    if C.max_leaf_size < 1 then invalid_arg "max_leaf_size must be at least 1."
    else C.max_leaf_size

  let min_leaf_size = (max_leaf_size + 1) / 2

  let (^^) = C.append

  let split_container c i = C.sub c 0 i, C.sub c i (C.length c - i)

  let size : type n. ('a, n) node -> int = function
    | Leaf c -> C.length c
    | Two (_, _, size) -> size
    | Three (_, _, _, size) -> size

  let length = function
    | Empty -> 0
    | Flat c -> C.length c
    | Node node -> size node

  let flat c =
    let len = C.length c in assert (len > 0 && len < min_leaf_size);
    Flat c

  let leaf c =
    let len = C.length c in assert (len >= min_leaf_size && len <= max_leaf_size);
    Leaf c

  let leaf_flat_empty c =
    let len = C.length c in
    if len = 0 then Empty else if len < min_leaf_size then flat c else Node (leaf c)

  let two l r = Two (l, r, size l + size r)

  let three l m r = Three (l, m, r, size l + size m + size r)

  let subtree_at (type n) i : ('a, n s) node -> ('a, n) node * int = function
    | Two (l, r, _) ->
      let size_l = size l in
      if i < size_l then (l, i)
      else (r, i - size_l)
    | Three (l, m, r, _) ->
      let size_l = size l in
      let size_l_m = size_l + size m in
      if i < size_l then (l, i)
      else if i < size_l_m then (m, i - size_l)
      else (r, i - size_l_m)

  let get i t =
    if i < 0 || i >= length t then None
    else match t with
      | Empty -> None
      | Flat c -> Some (C.get c i)
      | Node node ->
        let rec get : type n. int -> ('a, n) node -> 'a option = fun i -> function
          | Leaf c -> Some (C.get c i)
          | Two _ as node -> let node, i = node |> subtree_at i in node |> get i
          | Three _ as node -> let node, i = node |> subtree_at i in node |> get i
        in
        node |> get i

  let get_exn i t = t |> get i |> extract_exn "get_exn"

  type ('a, 'n) append_result =
    | Done of ('a, 'n) node
    | Up of ('a, 'n) node * ('a, 'n) node

  type ('a, _) one_node =
    | One : ('a, 'n) one_node -> ('a, 'n s) one_node
    | Wrap : ('a, 'n) node -> ('a, 'n) one_node
    | Short : 'a C.t -> ('a, z) one_node

  (* Requires that C.length c1 + C.length c2 is between min_leaf_size and 2 * max_leaf_size. *)
  let append_leaf_and_short c1 c2 =
    let l1 = C.length c1 in let l2 = C.length c2 in let len = l1 + l2 in
    if l1 = 0 then Done (leaf c2) else if l2 = 0 then Done (leaf c1)
    else if len <= max_leaf_size then Done (leaf (c1 ^^ c2))
    else
      let m = len / 2 in
      let c1, c2 =
        if l1 < m then let c2', c2'' = split_container c2 (m - l1) in c1 ^^ c2', c2''
        else if l1 > m then let c1', c1'' = split_container c1 m in c1', c1'' ^^ c2
        else c1, c2
      in Up (leaf c1, leaf c2)

  let rec append_right : type n. ('a, n) node -> ('a, n) one_node ->
    ('a, n) append_result = fun node one_node -> match node, one_node with
    | Leaf c1, Short c2 -> append_leaf_and_short c1 c2
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

  let rec append_left : type n. ('a, n) one_node -> ('a, n) node ->
    ('a, n) append_result = fun node one_node -> match node, one_node with
    | Short c1, Leaf c2 -> append_leaf_and_short c1 c2
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

  type ('a, _) down_node =
    | Start : ('a, 'n) node -> ('a, 'n) down_node
    | Down : ('a, 'n s) down_node -> ('a, 'n) down_node

  type 'a balanced_pair =
    | Balanced : ('a, 'n) node * ('a, 'n) node -> 'a balanced_pair
    | Left : ('a, 'n) one_node * ('a, 'n) node -> 'a balanced_pair
    | Right : ('a, 'n) node * ('a, 'n) one_node -> 'a balanced_pair

  let append ~l ~r =
    let down node =
      let rec down : type n. ('a, n) down_node -> ('a, n) node -> ('a, z) down_node = fun d ->
        function
        | Leaf _ -> d
        | Two (sub, _, _) -> down (Down d) sub
        | Three (sub, _, _, _) -> down (Down d) sub
      in
      down (Start node) node
    in
    let rec balance_left : type n. ('a, n) one_node -> ('a, n) down_node ->
      'a balanced_pair = fun l -> function
      | Start r -> Left (l, r)
      | Down r -> balance_left (One l) r
    in
    let rec balance_right : type n. ('a, n) down_node -> ('a, n) one_node ->
      'a balanced_pair = fun l r -> match l with
      | Start l -> Right (l, r)
      | Down l -> balance_right l (One r)
    in
    let rec make_balanced_pair : type n. ('a, n) down_node -> ('a, n) down_node ->
      'a balanced_pair = fun l r -> match l, r with
      | Start l, Start r -> Balanced (l, r)
      | Down l, Down r -> make_balanced_pair l r
      | Down l, Start r -> balance_right l (One (Wrap r))
      | Start l, Down r -> balance_left (One (Wrap l)) r
    in
    let convert_append_result = function
      | Done node -> Node node
      | Up (l, r) -> Node (two l r)
    in
    let append_balanced_pair = function
      | Balanced (l, r) -> Node (two l r)
      | Left (l, r) -> append_left l r |> convert_append_result
      | Right (l, r) -> append_right l r |> convert_append_result
    in
    match l, r with
    | Empty, t | t, Empty -> t
    | Flat c1, Flat c2 -> leaf_flat_empty (c1 ^^ c2)
    | Flat c, Node node -> balance_left (Short c) (down node) |> append_balanced_pair
    | Node node, Flat c -> balance_right (down node) (Short c) |> append_balanced_pair
    | Node l, Node r -> make_balanced_pair (down l) (down r) |> append_balanced_pair

  let rec concat_same_height : type n. ('a, n) node list -> 'a t = fun nodes ->
    let rec reduce : type n. ('a, n) node -> ('a, n) node -> ('a, n) node list ->
      ('a, n s) node list -> ('a, n s) node list = fun l r nodes acc ->
      match nodes with
      | [] -> List.rev (two l r :: acc)
      | [r'] -> List.rev (three l r r' :: acc)
      | l'::r'::nodes -> reduce l' r' nodes (two l r :: acc)
    in
    match nodes with
    | [] -> Empty
    | [node] -> Node node
    | l::r::nodes -> reduce l r nodes [] |> concat_same_height

  let concat ts =
    let rec group_leaves ts acc = match ts with
      | Empty::ts -> group_leaves ts acc
      | Node (Leaf _ as leaf)::ts -> group_leaves ts (leaf::acc)
      | l -> List.rev acc |> concat_same_height, l
    in
    (* TODO should concat flats instead of appending individually. *)
    let rec concat l = function
      | [] -> l
      | Node (Leaf _ as leaf)::ts -> let r, ts = group_leaves ts [leaf] in concat (append ~l ~r) ts
      | r::ts -> concat (append ~l ~r) ts
    in
    ts |> concat Empty

  let split i t =
    if i < 0 || i > length t then None
    else if i = 0 then Some (Empty, t)
    else if i = length t then Some (t, Empty)
    else match t with
      | Empty -> Some (Empty, Empty)
      | Flat c -> let c1, c2 = split_container c i in Some (flat c1, flat c2)
      | Node node ->
        let rec split : type n. int -> ('a, n) node ->
          ('a, n) one_node * ('a, n) one_node = fun i node ->
          let convert = function
            | Done node -> One (Wrap node)
            | Up (l, r) -> Wrap (two l r)
          in
          match node with
          | Leaf c ->
            assert (i > 0 && i < C.length c);
            let short_or_leaf c = if C.length c < min_leaf_size then Short c else Wrap (leaf c) in
            let c1, c2 = split_container c i in short_or_leaf c1, short_or_leaf c2
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
        let rec convert_one_node : type n. ('a, n) one_node -> 'a t = function
          | Wrap node -> Node node
          | One o -> convert_one_node o
          | Short c -> flat c
        in
        let l, r = node |> split i in Some (convert_one_node l, convert_one_node r)

  let split_exn i t = t |> split i |> extract_exn "split_exn"

  let sub i j t =
    (* TODO sub is O(log n) but constant factors could be lower. *)
    match t |> split j with
    | None -> None
    | Some (t, _) ->
      match t |> split i with
      | None -> None
      | Some (_, t) -> Some t

  let sub_exn i j t = t |> sub i j |> extract_exn "sub_exn"

  (* Turns a container into a list of leaves, assuming C.length c > max_leaf_size. *)
  let list_leaves c =
    let rec list_leaves i acc =
      if i <= max_leaf_size * 2 then
        let i2 = i / 2 in leaf (C.sub c 0 i2) :: leaf (C.sub c i2 (i - i2)) :: acc
      else
        let i = i - max_leaf_size in list_leaves i @@ leaf (C.sub c i max_leaf_size) :: acc
    in
    list_leaves (C.length c) []

  let of_container c =
    if (C.length c) <= max_leaf_size then leaf_flat_empty c
    else list_leaves c |> concat_same_height

  let of_container_list cs =
    (* TODO should concat very short containers instead of appending individually. *)
    let rec make_leaves cs acc = match cs with
      | [] -> List.rev acc, None
      | [c] when C.length c < min_leaf_size -> List.rev acc, Some (Flat c)
      | c::c'::cs when C.length c < min_leaf_size ->
        (* TODO  *)
        make_leaves ((c ^^ c')::cs) acc
      | c::cs when C.length c <= max_leaf_size -> make_leaves cs (leaf c :: acc)
      | c::cs -> make_leaves cs @@ List.rev_append (list_leaves c) acc
    in
    let leaves, flat_o = make_leaves cs [] in
    let node = concat_same_height leaves in
    match flat_o with
    | None -> node
    | Some flat -> append ~l:node ~r:flat

  let flatten_map f = function
    | Empty -> []
    | Flat c -> [f c]
    | Node node ->
      let rec inorder : type n. ('a, n) node -> 'b list -> 'b list = fun xs acc ->
        match xs with
        | Leaf c -> f c :: acc
        | Two (l, r, _) -> acc |> inorder r |> inorder l
        | Three (l, m, r, _) -> acc |> inorder r |> inorder m |> inorder l
      in
      inorder node []

  let flatten t = flatten_map (fun x -> x) t

  let map_containers f_c = function
    | Empty -> Empty
    | Flat c -> Flat (f_c c)
    | Node node ->
      let rec map_node : type n. ('a, n) node -> ('b, n) node = function
        | Leaf c -> Leaf (f_c c)
        | Two (l, r, s) -> Two (map_node l, map_node r, s)
        | Three (l, m, r, s) -> Three (map_node l, map_node m, map_node r, s)
      in
      Node (map_node node)

  let rec repeat_to len = function
    | Empty -> invalid_arg "repeat_to - trying to repeat the empty tree."
    | Flat c -> leaf_flat_empty (c ^^ c) |> repeat_to len
    | Node node as t ->
      match t |> sub 0 len with
      | None -> Node (two node node) |> repeat_to len
      | Some t -> t

  module Iterator = struct
    type 'a t = {
      path_o : ('a, z) path option;
      c : 'a C.t;
      i : int;
      ended : bool;
    }

    and ('a, _) path =
      | Top : ('a, 'n s) node -> ('a, 'n s) path
      | Down : {
          path : ('a, 'n s) path;
          node : ('a, 'n) node;
          offset : int;
        } -> ('a, 'n) path

    let rec get : type n. int -> ('a, n) path -> 'a t = fun i path ->
      let down (type n) (node : ('a, n s) node) ~path ~offset =
        let node, i' = node |> subtree_at i in
        get i' (Down {path; node; offset = offset + i - i'})
      in
      match path with
      | Down {node=Leaf c; _} -> {path_o=Some path; c; i; ended = i >= C.length c}
      | Top (Two _ as node) -> down node ~path ~offset:0
      | Top (Three _ as node) -> down node ~path ~offset:0
      | Down {node=Two _ as node; offset; _} -> down node ~path ~offset
      | Down {node=Three _ as node; offset; _} -> down node ~path ~offset

    let move : type n. int -> ('a, n) path -> 'a t = fun i -> function
      | Top _ as path -> path |> get i
      | Down {path; offset; _} ->
        let rec move : type n. int -> ('a, n s) path -> 'a t = fun pos -> function
          | Top _ as path -> get pos path
          | Down {path=up_path; node; offset} as path ->
            let i = pos - offset in
            if i >= 0 && i < size node then get i path
            else move pos up_path
        in
        path |> move (offset + i)

    let start_at pos = function
      | Empty -> None
      | Flat c -> Some ({path_o=None; c; i=pos; ended=false})
      | Node (Leaf c) -> Some ({path_o=None; c; i=pos; ended=false})
      | Node (Two _ as node) -> Some ((Top node) |> get pos)
      | Node (Three _ as node) -> Some ((Top node) |> get pos)

    let is_at_end {ended; _} = ended

    let next ({path_o; c; i; ended} as t) =
      let i_last = C.length c - 1 in
      if i <= i_last then C.get c i, {t with i = i + 1}
      else if ended then C.get c i_last, t
      else match path_o with
        | None -> C.get c i_last, {t with ended=true}
        | Some path ->
          let ({c; i; ended; _} as t) = path |> move i in
          C.get c (if ended then C.length c - 1 else i), {t with i = i + 1}

    let skip n ({i; _} as t) = {t with i = i + n}
  end
end

module F_array = struct
  module Array_container = struct
    type 'a t = 'a array

    let max_leaf_size = 128

    let length = Array.length

    let get = Array.get

    let append = Array.append

    let sub = Array.sub
  end

  include Make (Array_container)

  let of_array = of_container

  let of_array_list = of_container_list

  let to_array t = t |> flatten |> Array.concat

  let of_list l = l |> Array.of_list |> of_array

  let make len a =
    if len <= max_leaf_size then of_array (Array.make len a)
    else Array.make max_leaf_size a |> of_array |> repeat_to len

  let map f = map_containers (Array.map f)
end

module Rope = struct
  type 'a str = Str : string -> char str

  module String_container = struct
    type 'a t = 'a str

    let max_leaf_size = 512

    let length : type a. a t -> int = fun (Str s) -> String.length s

    let get : type a. a t -> int -> a = fun (Str s) i -> String.get s i

    let sub : type a. a t -> int -> int -> a t = fun (Str s) i j -> Str (String.sub s i j)

    let append : type a. a t -> a t -> a t = fun (Str s1) (Str s2) -> Str (s1 ^ s2)
  end

  include Make (String_container)

  let of_string s = of_container (Str s)

  let of_char c = of_container (Str (String.make 1 c))

  let of_string_list ss = ss |> List.map (fun s -> Str s) |> of_container_list

  let to_string t = t |> flatten_map (fun (Str s) -> s) |> String.concat ""

  let make len c =
    if len <= max_leaf_size then of_string (String.make len c)
    else String.make max_leaf_size c |> of_string |> repeat_to len

  let map f = map_containers (fun (Str s) -> Str (String.map f s))

  let printer n fmt t =
    let t =
      if n < 0 then t
      else match t |> sub 0 n with
        | None -> t
        | Some l -> append ~l ~r:(of_string "...")
    in
    Format.fprintf fmt "<rope \"%s\">" (to_string t)

  let default_printer = printer 70
end

module One_tree = struct
  module One_container = struct
    type 'a t = 'a

    let max_leaf_size = 1

    let length _ = 1

    let get t i = assert (i = 0); t

    let append _ _ = assert false

    let sub _ _ _ = assert false
  end

  include Make (One_container)

  let singleton = of_container

  let of_list = of_container_list

  let to_list = flatten

  let make len a = singleton a |> repeat_to len

  let map f = map_containers f
end

module Quick_tree (T : S) = struct
  type 'a t = {
    ls : 'a T.t list;
    m : 'a T.t;
    rs : 'a T.t list; (* Stored reversed. *)
    len_ls : int;
    len_rs : int;
  }

  type 'a tree = 'a t

  let empty = {ls = []; m = T.empty; rs = []; len_ls = 0; len_rs = 0}

  let of_tree m = {empty with m}

  let to_tree {ls; m; rs; _} = T.append ~l:(T.concat ls) ~r:(T.concat (m::List.rev rs))

  let length {m; len_ls; len_rs; _} = len_ls + T.length m + len_rs

  let get i t = t |> to_tree |> T.get i

  let get_exn i t = t |> to_tree |> T.get_exn i

  let max_length = 1000

  let append ~l ~r =
    let full_append () = T.append ~l:(to_tree l) ~r:(to_tree r) |> of_tree in
    if l.len_rs = max_length || r.len_ls = max_length then full_append ()
    else
      (* TODO need to just cons on to list if it's short. *)
      full_append ()

  let concat ts =
    let rec concat l = function
      | [] -> l
      | r::ts -> concat (append ~l ~r) ts
    in concat empty ts

  let split i t = match t |> to_tree |> T.split i with
    | None -> None
    | Some (l, r) -> Some (of_tree l, of_tree r)

  let split_exn i t = let l, r = t |> to_tree |> T.split_exn i in of_tree l, of_tree r

  let sub i j t = match t |> to_tree |> T.sub i j with
    | None -> None
    | Some t -> Some (of_tree t)

  let sub_exn i j t = t |> to_tree |> T.sub_exn i j |> of_tree

  module Iterator = struct
    include T.Iterator

    let start_at i t = t |> to_tree |> T.Iterator.start_at i
  end
end

module Zipped_trees (L : S) (R : S) = struct
  type 'a t =
    | Empty : 'a t
    | Pair : 'a L.t * 'b R.t -> ('a * 'b) t

  type 'a tree = 'a t

  let empty = Empty

  let create ~l ~r = Pair (l, r)

  let left = function
    | Empty -> L.empty
    | Pair (l, _) -> l

  let right = function
    | Empty -> R.empty
    | Pair (_, r) -> r

  let length : type a. a t -> int = function
    | Empty -> 0
    | Pair (l, _) -> L.length l

  let get (type a) i : a t -> a option = function
    | Empty -> None
    | Pair (l, r) ->
      match L.get i l, R.get i r with
      | Some a, Some b -> Some (a, b)
      | _ -> None

  let get_exn (type a) i : a t -> a = function
    | Empty -> raise (Out_of_bounds "get_exn")
    | Pair (l, r) -> L.get_exn i l, R.get_exn i r

  let append (type a) ~(l : a t) ~(r : a t) : a t = match l, r with
    | t, Empty | Empty, t -> t
    | Pair (l1, r1), Pair (l2, r2) -> Pair (L.append ~l:l1 ~r:l2, R.append ~l:r1 ~r:r2)

  type (_, _) eql = Refl : ('a, 'a) eql

  let rec concat : type a. a t list -> a t = function
    | [] -> Empty
    | Empty::ts -> concat ts
    | Pair (l, r)::ts ->
      let rec concat_pairs : type a b c. (a, b * c) eql -> b L.t list -> c R.t list -> a t list ->
        a t = fun eql ls rs ts -> match ts, eql with
        | [], Refl -> Pair (L.concat (List.rev ls), R.concat (List.rev rs))
        | Empty::ts, _ -> concat_pairs eql ls rs ts
        | Pair (l, r)::ts, Refl -> concat_pairs eql (l::ls) (r::rs) ts
      in
      concat_pairs Refl [l] [r] ts

  let split (type a) i : a t -> (a t * a t) option = function
    | Empty -> if i = 0 then Some (Empty, Empty) else None
    | Pair (l, r) ->
      match L.split i l, R.split i r with
      | Some (l1, r1), Some (l2, r2) -> Some (Pair (l1, l2), Pair (r1, r2))
      | _ -> None

  let split_exn (type a) i (t : a t) : a t * a t = t |> split i |> extract_exn "split_exn"

  let sub (type a) i j : a t -> a t option = function
    | Empty -> if i = 0 && j = 0 then Some Empty else None
    | Pair (l, r) ->
      match L.sub i j l, R.sub i j r with
      | Some l, Some r -> Some (Pair (l, r))
      | _ -> None

  let sub_exn (type a) i j (t : a t) : a t = t |> sub i j |> extract_exn "sub_exn"

  module Iterator = struct
    type 'a t = Iter : 'a L.Iterator.t * 'b R.Iterator.t -> ('a * 'b) t

    let start_at (type a) i : a tree -> a t option = function
      | Empty -> None
      | Pair (l, r) ->
        match L.Iterator.start_at i l, R.Iterator.start_at i r with
        | Some l_iter, Some r_iter -> Some (Iter (l_iter, r_iter))
        | _ -> None

    let is_at_end (type a) (Iter (l_iter, r_iter) : a t) =
      L.Iterator.is_at_end l_iter && R.Iterator.is_at_end r_iter

    let next (type a) (Iter (l_iter, r_iter) : a t) : a * a t =
      let a, l_iter = L.Iterator.next l_iter in
      let b, r_iter = R.Iterator.next r_iter in
      (a, b), Iter (l_iter, r_iter)

    let skip (type a) n (Iter (l_iter, r_iter) : a t) : a t =
      Iter (L.Iterator.skip n l_iter, R.Iterator.skip n r_iter)
  end
end
