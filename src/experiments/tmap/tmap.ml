type tree = Bin of tree * tree | Tip of int
type tzipper = Top | BinL of tzipper * tree | BinR of tree * tzipper

let rec down (t : tree) (f : int -> int) (ctx : tzipper) : tree =
  match t with
  | Bin (l, r) -> down l f (BinL (ctx, r))
  | Tip x ->
      let fx = f x in
      app (Tip fx) f ctx

and app (t : tree) (f : int -> int) (ctx : tzipper) : tree =
  match ctx with
  | Top -> t
  | BinR (l, up) -> app (Bin (l, t)) f up
  | BinL (up, r) -> down r f (BinR (t, up))

and tmap (t : tree) (f : int -> int) : tree = down t f Top

let add_5 (n : int) : int = n + 5

let rec fold_sum (t : tree) : int =
  match t with
  | Tip x -> x
  | Bin (l, r) ->
      let res_l = fold_sum l in
      let res_r = fold_sum r in
      res_l + res_r

let negate (n : int) : int = -n

let rec create_tree (n : int) (layer : int) : tree =
  if layer == 1 then Tip 1
  else
    let two_n_minus_1 = (2 * n) - 1 in
    let two_n = 2 * n in
    let pred_layer = layer - 1 in
    let res_l = create_tree two_n_minus_1 pred_layer in
    let res_r = create_tree two_n pred_layer in
    Bin (res_l, res_r)

let rec first_el (t : tree) : int =
  match t with Bin (l, _) -> first_el l | Tip x -> x
;;

let t = create_tree 1 30 in
let t2 = tmap t negate in
print_int (fold_sum t2)
