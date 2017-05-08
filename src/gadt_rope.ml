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

include struct [@@@warning "-37"]
  type z

  type 'n s = S (* As S isn't used it causes warning 37. *)
end

type _ node =
  | Leaf : string -> z node
  | Two : 'n node * 'n node * int -> 'n s node
  | Three : 'n node * 'n node * 'n node * int -> 'n s node

type t =
  | Empty : t
  | Flat : string -> t
  | Node : 'n node -> t

type rope = t

let empty = Empty

let max_leaf_size = 512

let min_leaf_size = (max_leaf_size + 1) / 2

let split_string s i = String.sub s 0 i, String.sub s i (String.length s - i)

let size : type n. n node -> int = function
  | Leaf s -> String.length s
  | Two (_, _, size) -> size
  | Three (_, _, _, size) -> size

let length = function
  | Empty -> 0
  | Flat s -> String.length s
  | Node node -> size node

let flat s =
  let len = String.length s in assert (len > 0 && len < min_leaf_size);
  Flat s

let leaf s =
  let len = String.length s in assert (len >= min_leaf_size && len <= max_leaf_size);
  Leaf s

let leaf_flat_empty s =
  let len = String.length s in
  if len = 0 then Empty else if len < min_leaf_size then flat s else Node (leaf s)

let two l r = Two (l, r, size l + size r)

let three l m r = Three (l, m, r, size l + size m + size r)

let subtree_at (type n) i : n s node -> n node * int = function
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
    | Flat s -> Some (String.get s i)
    | Node node ->
      let rec get : type n. int -> n node -> 'a option = fun i -> function
        | Leaf s -> Some (String.get s i)
        | Two _ as node -> let node, i = node |> subtree_at i in node |> get i
        | Three _ as node -> let node, i = node |> subtree_at i in node |> get i
      in
      node |> get i

let get_exn i t = t |> get i |> extract_exn "get_exn"

type 'n append_result =
  | Done of 'n node
  | Up of 'n node * 'n node

type _ one_node =
  | One : 'n one_node -> 'n s one_node
  | Wrap : 'n node -> 'n one_node
  | Short : string -> z one_node

(* Requires that length s1 + length s2 is between min_leaf_size and 2 * max_leaf_size. *)
let append_leaf_and_short s1 s2 =
  let l1 = String.length s1 in let l2 = String.length s2 in let len = l1 + l2 in
  if l1 = 0 then Done (leaf s2) else if l2 = 0 then Done (leaf s1)
  else if len <= max_leaf_size then Done (leaf (s1 ^ s2))
  else
    let m = len / 2 in
    let s1, s2 =
      if l1 < m then let s2', s2'' = split_string s2 (m - l1) in s1 ^ s2', s2''
      else if l1 > m then let s1', s1'' = split_string s1 m in s1', s1'' ^ s2
      else s1, s2
    in Up (leaf s1, leaf s2)

let rec append_right : type n. n node -> n one_node -> n append_result = fun node one_node ->
  match node, one_node with
  | Leaf s1, Short s2 -> append_leaf_and_short s1 s2
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

let rec append_left : type n. n one_node -> n node -> n append_result = fun node one_node ->
  match node, one_node with
  | Short s1, Leaf s2 -> append_leaf_and_short s1 s2
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

type _ down_node =
  | Start : 'n node -> 'n down_node
  | Down : 'n s down_node -> 'n down_node

type balanced_pair =
  | Balanced : 'n node * 'n node -> balanced_pair
  | Left : 'n one_node * 'n node -> balanced_pair
  | Right : 'n node * 'n one_node -> balanced_pair

let append ~l ~r =
  let down node =
    let rec down : type n. n down_node -> n node -> z down_node = fun d ->
      function
      | Leaf _ -> d
      | Two (sub, _, _) -> down (Down d) sub
      | Three (sub, _, _, _) -> down (Down d) sub
    in
    down (Start node) node
  in
  let rec balance_left : type n. n one_node -> n down_node -> balanced_pair = fun l r ->
    match r with
    | Start r -> Left (l, r)
    | Down r -> balance_left (One l) r
  in
  let rec balance_right : type n. n down_node -> n one_node -> balanced_pair = fun l r ->
    match l with
    | Start l -> Right (l, r)
    | Down l -> balance_right l (One r)
  in
  let rec make_balanced_pair : type n. n down_node -> n down_node ->
    balanced_pair = fun l r -> match l, r with
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
  | Flat s1, Flat s2 -> leaf_flat_empty (s1 ^ s2)
  | Flat s, Node node -> balance_left (Short s) (down node) |> append_balanced_pair
  | Node node, Flat s -> balance_right (down node) (Short s) |> append_balanced_pair
  | Node l, Node r -> make_balanced_pair (down l) (down r) |> append_balanced_pair

let (<^>) l r = append ~l ~r

let split i t =
  if i < 0 || i > length t then None
  else if i = 0 then Some (Empty, t)
  else if i = length t then Some (t, Empty)
  else match t with
    | Empty -> Some (Empty, Empty)
    | Flat s -> let s1, s2 = split_string s i in Some (flat s1, flat s2)
    | Node node ->
      let rec split : type n. int -> n node ->
        n one_node * n one_node = fun i node ->
        let convert = function
          | Done node -> One (Wrap node)
          | Up (l, r) -> Wrap (two l r)
        in
        match node with
        | Leaf s ->
          assert (i > 0 && i < String.length s);
          let short_leaf s = if String.length s < min_leaf_size then Short s else Wrap (leaf s) in
          let s1, s2 = split_string s i in short_leaf s1, short_leaf s2
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
      let rec convert_one_node : type n. n one_node -> t = function
        | Wrap node -> Node node
        | One o -> convert_one_node o
        | Short s -> flat s
      in
      let l, r = node |> split i in Some (convert_one_node l, convert_one_node r)

let split_exn i t = t |> split i |> extract_exn "split_exn"

let sub i j t =
  match t |> split j with
  | None -> None
  | Some (t, _) ->
    match t |> split i with
    | None -> None
    | Some (_, t) -> Some t

let sub_exn i j t = t |> sub i j |> extract_exn "sub_exn"

let rec concat_same_height : type n. n node list -> t = fun nodes ->
  let rec reduce : type n. n node -> n node -> n node list ->
    n s node list -> n s node list = fun l r nodes acc ->
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

(* Turns a string into a list of leaves, assuming String.length s > max_leaf_size. *)
let list_leaves s =
  let rec list_leaves i acc =
    if i <= max_leaf_size * 2 then
      let i2 = i / 2 in leaf (String.sub s 0 i2) :: leaf (String.sub s i2 (i - i2)) :: acc
    else
      let i = i - max_leaf_size in list_leaves i @@ leaf (String.sub s i max_leaf_size) :: acc
  in
  list_leaves (String.length s) []

let of_string s =
  if String.length s <= max_leaf_size then leaf_flat_empty s
  else list_leaves s |> concat_same_height

let to_string = function
    | Empty -> ""
    | Flat s -> s
    | Node node ->
      let rec inorder : type n. n node -> string list -> string list = fun node acc ->
        match node with
        | Leaf s -> s :: acc
        | Two (l, r, _) -> inorder l @@ inorder r acc
        | Three (l, m, r, _) -> inorder l @@ inorder m @@ inorder r acc
      in
      inorder node [] |> String.concat ""

module Iterator = struct
  type iter = int * info

  and info = {
    path_o : z path option;
    s : string;
    ended : bool;
  }

  and _ path =
    | Top : 'n s node -> 'n s path
    | Down : {
        path : 'n s path;
        node : 'n node;
        offset : int;
      } -> 'n path

  let rec get : type n. int -> n path -> iter = fun i path ->
    let down (type n) (node : n s node) ~path ~offset =
      let node, i' = node |> subtree_at i in
      get i' (Down {path; node; offset = offset + i - i'})
    in
    match path with
    | Down {node=Leaf s; _} -> (i, {path_o = Some path; s; ended = i >= String.length s})
    | Top (Two _ as node) -> down node ~path ~offset:0
    | Top (Three _ as node) -> down node ~path ~offset:0
    | Down {node=Two _ as node; offset; _} -> down node ~path ~offset
    | Down {node=Three _ as node; offset; _} -> down node ~path ~offset

  let move : type n. int -> n path -> iter = fun i -> function
    | Top _ as path -> path |> get i
    | Down {path; offset; _} ->
      let rec move : type n. int -> n s path -> iter = fun pos -> function
        | Top _ as path -> get pos path
        | Down {path=up_path; node; offset} as path ->
          let i = pos - offset in
          if i >= 0 && i < size node then get i path
          else move pos up_path
      in
      path |> move (offset + i)

  let start_at pos = function
    | Empty -> None
    | Flat s -> Some (pos, {path_o = None; s; ended = false})
    | Node (Leaf s) -> Some (pos, {path_o = None; s; ended = false})
    | Node (Two _ as node) -> Some ((Top node) |> get pos)
    | Node (Three _ as node) -> Some ((Top node) |> get pos)

  let is_at_end (_, {ended; _}) = ended

  let next (i, ({path_o; s; ended} as info) as t) =
    let i_last = String.length s - 1 in
    if i <= i_last then String.get s i, (i + 1, info)
    else if ended then String.get s i_last, t
    else match path_o with
      | None -> String.get s i_last, (i, {info with ended = true})
      | Some path ->
        let (i, ({s; ended; _} as info)) = path |> move i in
        String.get s (if ended then String.length s - 1 else i), (i + 1, info)

  let skip n (i, info) = (i + n, info)
end

let printer n = fun fmt t ->
  Format.fprintf fmt "<rope \"%s\">" @@ to_string @@
  if n < 0 || length t < n then t
  else if n < 10 then sub_exn 0 n t
  else sub_exn 0 (n - 3) t <^> of_string "..."

let default_printer = printer 70
