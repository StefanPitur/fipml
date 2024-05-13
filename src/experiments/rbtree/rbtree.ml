type color = Red | Black
type tree = Node of color * tree * int * bool * tree | Leaf
type balance_node = Balance of color * tree * int * bool * tree

type accum =
  | Done
  | NodeL of color * accum * int * bool * tree
  | NodeR of color * tree * int * bool * accum

let rec rebuild z t =
  match z with
  | NodeR (c, l, k, v, z1) -> rebuild z1 (Node (c, l, k, v, t))
  | NodeL (c, z1, k, v, r) -> rebuild z1 (Node (c, t, k, v, r))
  | Done -> t

let rec balance z t =
  match t with
  | Balance (_, l, k, v, r) -> (
      match z with
      | NodeR (Black, l1, k1, v1, z1) ->
          rebuild z1 (Node (Black, l1, k1, v1, Node (Red, l, k, v, r)))
      | NodeL (Black, z1, k1, v1, r1) ->
          rebuild z1 (Node (Black, Node (Red, l, k, v, r), k1, v1, r1))
      | NodeR (Red, l1, k1, v1, z1) -> (
          match z1 with
          | NodeR (_, l2, k2, v2, z2) ->
              balance z2
                (Balance
                   ( Black,
                     Node (Black, l2, k2, v2, l1),
                     k1,
                     v1,
                     Node (Black, l, k, v, r) ))
          | NodeL (_, z2, k2, v2, r2) ->
              balance z2
                (Balance
                   ( Black,
                     Node (Black, l1, k1, v1, l),
                     k,
                     v,
                     Node (Black, r, k2, v2, r2) ))
          | Done -> Node (Black, l1, k1, v1, Node (Red, l, k, v, r)))
      | NodeL (Red, z1, k1, v1, r1) -> (
          match z1 with
          | NodeR (_, l2, k2, v2, z2) ->
              balance z2
                (Balance
                   ( Black,
                     Node (Black, l2, k2, v2, l),
                     k,
                     v,
                     Node (Black, r, k1, v1, r1) ))
          | NodeL (_, z2, k2, v2, r2) ->
              balance z2
                (Balance
                   ( Black,
                     Node (Black, l, k, v, r),
                     k1,
                     v1,
                     Node (Black, r1, k2, v2, r2) ))
          | Done -> Node (Black, Node (Red, l, k, v, r), k1, v1, r1))
      | Done -> Node (Black, l, k, v, r))

let rec ins t k v z =
  match t with
  | Node (c, l, kx, vx, r) ->
      if k < kx then ins l k v (NodeL (c, z, kx, vx, r))
      else if k > kx then ins r k v (NodeR (c, l, kx, vx, z))
      else rebuild z (Node (c, l, kx, vx, r))
  | Leaf -> balance z (Balance (Black, Leaf, k, v, Leaf))

let insert t k v = ins t k v Done

let rec fold t b f =
  match t with
  | Node (_, l, k, v, r) -> fold r (f k v (fold l b f)) f
  | Leaf -> b

let rec make_tree_aux n t =
  if n <= 0 then t
  else
    let n1 = n - 1 in
    make_tree_aux n1 (insert t n1 (if n1 mod 10 == 0 then true else false))

let make_tree n = make_tree_aux n Leaf
let test_iter_fun (k : int) (v : bool) (r : int) : int = if v then r + 1 else r

let rec test_iter (n : int) (iter : int) : int =
  if iter == 0 then 0
  else
    let t = make_tree n in
    let fold_res = fold t 0 test_iter_fun in
    let pred_iter = iter - 1 in
    fold_res + test_iter n pred_iter

let test (n : int) : int =
  let div = if n <= 1 then 1 else n in
  let steps = 100000000 / div in
  test_iter n steps

let _ = print_int (test 100000)
