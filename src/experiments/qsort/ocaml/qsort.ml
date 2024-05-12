type pad = Pad
type unit2 = Unit2 of pad * pad
type maybe2 = Nothing2 | Just2 of int * pad
type sublist = SCons of int * sublist | STuple of int * int

type partition =
  | Sublist of sublist * partition
  | Singleton of int * partition
  | End

type int_list = Nil | Cons of int * int_list
type accum = MkLo of int * accum | MkHi of int * accum | Done

let rec quicksort (xs : int_list) : int_list = quicksort_go xs End

and quicksort_go (xs : int_list) (b : partition) : int_list =
  match xs with
  | Cons (p, xx) ->
      let lo, hi = split_list p xx Done b (Unit2 (Pad, Pad)) in
      quicksort_go lo hi
  | Nil -> quicksort_app b

and quicksort_app (bdl : partition) : int_list =
  match bdl with
  | Singleton (p, b) -> Cons (p, quicksort_app b)
  | Sublist (xs, bdl') -> (
      match xs with
      | SCons (p, xx) ->
          let lo, hi =
            split_sublist p xx Done bdl' (Unit2 (Pad, Pad)) (Unit2 (Pad, Pad))
          in
          quicksort_go lo hi
      | STuple (a, b) ->
          if a <= b then Cons (a, Cons (b, quicksort_app bdl'))
          else Cons (b, Cons (a, quicksort_app bdl')))
  | End -> Nil

and split_list (p : int) (xs : int_list) (k : accum) (b : partition) (u : unit2)
    : int_list * partition =
  match xs with
  | Cons (x, xx) ->
      if x < p then split_list p xx (MkLo (x, k)) b u
      else split_list p xx (MkHi (x, k)) b u
  | Nil ->
      let lo, hi = split_app1 k Nil Nothing2 b in
      (lo, Singleton (p, hi))

and split_sublist (p : int) (xs : sublist) (k : accum) (b : partition)
    (u : unit2) (u1 : unit2) : int_list * partition =
  match xs with
  | SCons (x, xx) ->
      if x < p then split_sublist p xx (MkLo (x, k)) b u u1
      else split_sublist p xx (MkHi (x, k)) b u u1
  | STuple (x, y) -> split_list p (Cons (x, Cons (y, Nil))) k b u

and split_app1 (k : accum) (lo : int_list) (hi : maybe2) (b : partition) :
    int_list * partition =
  match k with
  | MkLo (x, k) -> split_app1 k (Cons (x, lo)) hi b
  | MkHi (x, k) -> (
      match hi with
      | Nothing2 -> split_app1 k lo (Just2 (x, Pad)) b
      | Just2 (y, _) -> split_app2 k lo (STuple (y, x)) b (Unit2 (Pad, Pad)))
  | Done -> (
      match hi with
      | Just2 (x, _) -> (lo, Singleton (x, b))
      | Nothing2 -> (lo, b))

and split_app2 (k : accum) (lo : int_list) (hi : sublist) (b : partition)
    (u : unit2) : int_list * partition =
  match k with
  | MkLo (x, k) -> split_app2 k (Cons (x, lo)) hi b u
  | MkHi (x, k) -> split_app2 k lo (SCons (x, hi)) b u
  | Done -> (lo, Sublist (hi, b))

and rand_list (n : int) : int_list =
  if n > 0 then
    let pred_n = n - 1 in
    let rand_list_res = rand_list pred_n in
    Cons (n, rand_list_res)
  else Nil

let first_element (xs : int_list) (d : int) : int =
  match xs with Nil -> d | Cons (x, _) -> x

let rec test_iter (n : int) (iter : int) : int =
  if iter == 0 then 0
  else
    let xs = rand_list n in
    let ys = quicksort xs in
    let first_element_ys = first_element ys 0 in
    let pred_iter = iter - 1 in
    first_element_ys + test_iter n pred_iter

let test (n : int) (rounds : int) : int = test_iter n rounds
let _ = print_int (test 10000 100)
