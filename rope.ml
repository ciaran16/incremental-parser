(*
TODO
http://www.cs.nyu.edu/courses/fall06/G22.3520-001/lec5.pdf
- Insert anywhere in rope.
*)

type z = Z : z

type 'n s = S : 'n -> 'n s

type ('a, _) tree =
  | Leaf : 'a -> ('a, z) tree
  | Two : ('a, 'n) tree * ('a, 'n) tree * int -> ('a, 'n s) tree
  | Three : ('a, 'n) tree * ('a, 'n) tree * ('a, 'n) tree * int -> ('a, 'n s) tree

type 'a t =
  | Empty : 'a t
  | Tree : ('a, 'n) tree -> 'a t

let size : type n. ('a, n) tree -> int = function
  | Leaf _ -> 1
  | Two (_, _, size) -> size
  | Three (_, _, _, size) -> size

let length = function
  | Empty -> 0
  | Tree tree -> size tree

let leaf v = Leaf v

let two l r = Two (l, r, size l + size r)

let three l m r = Three (l, m, r, size l + size m + size r)

let empty = Empty

let of_list vs =
  let rec reduce1 : type n. ('a, n) tree list -> ('a, n s) tree list = function
    | [] -> []
    | [_] -> assert false
    | [l; m; r] -> [three l m r]
    | l::r::nodes -> two l r :: reduce1 nodes
  in
  let rec reduce : type n. ('a, n) tree list -> 'a t = function
    | [] -> Empty
    | [tree] -> Tree tree
    | nodes -> nodes |> reduce1 |> reduce
  in
  vs |> List.map leaf |> reduce

let of_string s =
  let rec explode i l = if i < 0 then l else explode (i - 1) (s.[i] :: l) in
  explode (String.length s - 1) [] |> of_list

let subtree_at (type n) i : ('a, n s) tree -> ('a, n) tree * int = function
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

let get i = function
  | Empty -> None
  | Tree tree ->
    let rec get : type n. int -> ('a, n) tree -> 'a option = fun i -> function
      | Leaf v -> if i = 0 then Some v else None
      | Two _ as node -> let node, i = node |> subtree_at i in node |> get i
      | Three _ as node -> let node, i = node |> subtree_at i in node |> get i
    in
    tree |> get i

let out_of_bounds = Invalid_argument "Index out of bounds."

type ('a, 'n) concat_result =
  | Done of ('a, 'n) tree
  | Up of ('a, 'n) tree * ('a, 'n) tree

let insert_exn i v = function
  | Empty -> Tree (leaf v)
  | Tree tree ->
    let rec insert : type n. int -> ('a, n) tree -> ('a, n) concat_result = fun i -> function
      | Leaf _ as leaf_r when i = 0 -> Up (leaf v, leaf_r)
      | Leaf _ as leaf_l when i = 1 -> Up (leaf_l, leaf v)
      | Leaf _ -> raise out_of_bounds
      | Two (l, r, _) when i < size l ->
        begin match l |> insert i with
          | Done l -> Done (two l r)
          | Up (l, m) -> Done (three l m r)
        end
      | Two (l, r, _) ->
        begin match r |> insert (i - size l) with
          | Done r -> Done (two l r)
          | Up (m, r) -> Done (three l m r)
        end
      | Three (l, m, r, _) when i < size l ->
        begin match l |> insert i with
          | Done l -> Done (three l m r)
          | Up (l, l') -> Up (two l l', two m r)
        end
      | Three (l, m, r, _) when i < size l + size m ->
        begin match m |> insert (i - size l) with
          | Done m -> Done (three l m r)
          | Up (m, m') -> Up (two l m, two m' r)
        end
      | Three (l, m, r, _) ->
        begin match r |> insert (i - size l - size m) with
          | Done r -> Done (three l m r)
          | Up (r, r') -> Up (two l m, two r r')
        end
      in
      match tree |> insert i with
      | Done tree -> Tree tree
      | Up (t1, t2) -> Tree (two t1 t2)

(*
type ('a, _) descent =
  | Start : ('a, 'n) tree -> ('a, 'n) descent
  | Down : ('a, 'n s) descent * ('a, 'n) tree -> ('a, 'n) descent

let descend f node = f (Start node) node

let down f d node = f (Down (d, node)) node

let rec left : type n. ('a, n) descent -> ('a, n) tree -> ('a, z) descent = fun d -> function
  | Leaf _ -> d
  | Two (l, _, _) -> down left d l
  | Three (l, _, _, _) -> down left d l

let rec right : type n. ('a, n) descent -> ('a, n) tree -> ('a, z) descent = fun d -> function
  | Leaf _ -> d
  | Two (l, _, _) -> down right d l
  | Three (l, _, _, _) -> down right d l

let rec join : type n. ('a, n) descent -> ('a, n) descent -> 'a t = fun dl dr ->
  match dl, dr with
  | Start l, Start r -> Tree (two l r)
  | Start l, Down (d, r) -> ()
  | Down (d, l), Start r -> ()
  | Down (dl, _), Down (dr, _) -> join dl dr

let concat t1 t2 =
  match t1, t2 with
  | Empty, (_ as t) | (_ as t), Empty -> t
  | Tree l, Tree r ->
    let dl = descend right l in
    let dr = descend left r in
    join dl dr
*)

type ('a, _) one_tree =
  | One : ('a, 'n) one_tree -> ('a, 'n s) one_tree
  | Wrap : ('a, 'n) tree -> ('a, 'n) one_tree

let rec concat_right : type n. ('a, n) tree -> ('a, n) one_tree -> ('a, n) concat_result =
  fun tree one_tree -> match tree, one_tree with
  | l, Wrap r -> Up (l, r)
  | Two (l, r, _), One r' ->
    begin match concat_right r r' with
      | Done r -> Done (two l r)
      | Up (m, r) -> Done (three l m r)
    end
  | Three (l, m, r, _), One r' ->
    begin match concat_right r r' with
      | Done r -> Done (three l m r)
      | Up (r, r') -> Up (two l m, two r r')
    end

let rec concat_left : type n. ('a, n) one_tree -> ('a, n) tree -> ('a, n) concat_result =
  fun tree one_tree -> match tree, one_tree with
  | Wrap l, r -> Up (l, r)
  | One l', Two (l, r, _) ->
    begin match concat_left l' l with
      | Done l -> Done (two l r)
      | Up (l, m) -> Done (three l m r)
    end
  | One l', Three (l, m, r, _) ->
    begin match concat_left l' l with
      | Done l -> Done (three l m r)
      | Up (l', l) -> Up (two l' l, two m r)
    end

let rec split : type n. int -> ('a, n) tree -> ('a, n) one_tree * ('a, n) one_tree = fun i node ->
  let convert = function
    | Done node -> One (Wrap node)
    | Up (l, r) -> Wrap (two l r)
  in
  match node with
  | Leaf _ -> raise out_of_bounds
  | Two (l, r, _) when i < size l ->
    let l, l' = l |> split i in
    One l, convert (concat_left l' r)
  | Two (l, r, _) when i = size l ->
    One (Wrap l), One (Wrap r)
  | Two (l, r, _) ->
    let r, r' = r |> split (i - size l) in
    convert (concat_right l r), One r'
  | Three (l, m, r, _) when i < size l ->
    let l, l' = l |> split i in
    let r =
      match concat_left l' m with
      | Done l -> two l r
      | Up (l, m) -> three l m r
    in
    One l, Wrap r
  | Three (l, m, r, _) when i = size l ->
    One (Wrap l), Wrap (two m r)
  | Three (l, m, r, _) when i < size l + size m ->
    let m, m' = m |> split (i - size l) in
    convert (concat_right l m), convert (concat_left m' r)
  | Three (l, m, r, _) when i = size l + size m ->
    Wrap (two l m), One (Wrap r)
  | Three (l, m, r, _) ->
    let r, r' = r |> split (i - size l - size m) in
    let l =
      match concat_right m r with
      | Done r -> two l r
      | Up (m, r) -> three l m r
    in
    Wrap l, One r'

type ('a, _) delete_result =
  | Done : ('a, 'n) tree -> ('a, 'n) delete_result
  | Deleted_leaf : ('a, z) delete_result
  | Hole : ('a, 'n) tree -> ('a, 'n s) delete_result

let delete_exn i = function
  | Empty -> raise out_of_bounds
  | Tree tree ->
    let rec delete : type n. int -> ('a, n) tree -> ('a, n) delete_result = fun i -> function
      | Leaf _ -> if i = 0 then Deleted_leaf else raise out_of_bounds
      | Two (l, r, _) when i < size l ->
        begin match l |> delete i, r with
          | Done l, r -> Done (two l r)
          | Deleted_leaf, Leaf _ -> Hole r
          | Hole h, Two (l, r, _) -> Hole (three h l r)
          | Hole h, Three (l, m, r, _) -> Done (two (two h l) (two m r))
        end
      | Two (l, r, _) ->
        begin match l, r |> delete (i - size l) with
          | l, Done r -> Done (two l r)
          | Leaf _, Deleted_leaf -> Hole l
          | Two (l, r, _), Hole h -> Hole (three l r h)
          | Three (l, m, r, _), Hole h -> Done (two (two l m) (two r h))
        end
      | Three (l, m, r, _) when i < size l ->
        begin match l |> delete i, m with
          | Done l, r -> Done (three l m r)
          | Deleted_leaf, Leaf _ -> Done (two m r)
          | Hole h, Two (l', r', _) -> Done (two (three h l' r') r)
          | Hole h, Three (l', m', r', _) -> Done (three (two h l') (two m' r') r)
        end
      | Three (l, m, r, _) when i < size l + size m ->
        begin match l, m |> delete (i - size l) with
          | l, Done m -> Done (three l m r)
          | Leaf _, Deleted_leaf -> Done (two l r)
          | Two (l', r', _), Hole h -> Done (two (three l' r' h) r)
          | Three (l', m', r', _), Hole h -> Done (three (two l' m') (two r' h) r)
        end
      | Three (l, m, r, _) ->
        begin match m, r |> delete (i - size l - size m) with
          | m, Done r -> Done (three l m r)
          | Leaf _, Deleted_leaf -> Done (two l m)
          | Two (l', r', _), Hole h -> Done (two l (three l' r' h))
          | Three (l', m', r', _), Hole h -> Done (three l (two l' m') (two r' h))
        end
    in
    match tree |> delete i with
    | Deleted_leaf -> Empty
    | Done tree -> Tree tree
    | Hole tree -> Tree tree

module Iterator = struct
  type 'a t =
    | Empty_walk : 'a t
    | Walk : {
        tree : ('a, _) tree;
        path : 'a path_item list;
        pos : int;
      } -> 'a t

  and 'a path_item =
    | P : {
        left : int;
        node : ('a, 'n) tree
      } -> 'a path_item

  let make_at pos = function
    | Empty -> Empty_walk
    | Tree tree -> Walk {tree; path=[]; pos}

  let change_to (type n) ~(tree : ('a, n) tree) ~path pos =
    let rec up (P {left; node} as hd) tl =
      let i = pos - left in
      if i >= 0 && i < size node then hd::tl
      else
        match tl with
        | [] -> [] (* Already at the top of the tree so can't go higher. *)
        | hd::tl -> up hd tl (* Move up one level. *)
    in
    let rec down (P {left; node} as hd) tl i =
      let f node =
        let node, i' = node |> subtree_at i in
        down (P {left=left + i - i'; node}) (hd::tl) i'
      in
      match node with
      | Leaf v -> v, tl
      | Two _ -> f node
      | Three _ -> f node
    in
    let path =
      match path with
      | [] -> up (P {left=0; node=tree}) []
      | hd::tl -> up hd tl
    in
    match path with
    | [] -> None, []
    | (P {left; _} as hd)::tl -> let v, path = down hd tl (pos - left) in Some v, path

  let next = function
    | Empty_walk -> None, Empty_walk
    | Walk {tree; path; pos} ->
      let o, path = change_to pos ~tree ~path in o, Walk {tree; path; pos=pos + 1}

  let rec take n t =
    match t |> next with
    | Some x, t when n > 0 -> let xs, t = t |> take (n - 1) in x::xs, t
    | _ -> [], t

  let drop n = function
    | Empty_walk -> Empty_walk
    | Walk ({pos; _} as w) -> Walk {w with pos=pos + n}
end

let iterator t = Iterator.make_at 0 t

let iterator_at pos t = Iterator.make_at pos t
