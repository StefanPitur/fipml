type pad = Pad
type unit2 = Unit2 of pad * pad
type pair = Pair of int * int
type sublist = SCons of int * sublist | STuple of int * int

type partition =
  | Sublist of sublist * partition
  | Singleton of int * partition
  | End

type int_list = Nil | Cons of int * int_list

let rec reverse_go (c : sublist) (acc : sublist) (u : unit2) : sublist =
  match c with
  | SCons (a, cs) -> reverse_go cs (SCons (a, acc)) u
  | STuple (a, b) -> (
      match u with Unit2 (Pad, Pad) -> SCons (b, SCons (a, acc)))

let reverse_sublist (sublst : sublist) : sublist =
  match sublst with
  | SCons (a, SCons (b, c)) -> reverse_go c (STuple (b, a)) (Unit2 (Pad, Pad))
  | SCons (a, STuple (b, c)) -> SCons (c, STuple (b, a))
  | STuple (a, b) -> STuple (b, a)

let rec sequences (xs : int_list) : partition =
  match xs with
  | Cons (a, Cons (b, xs1)) ->
      if a > b then
        let sublist, bs = descending b (STuple (b, a)) xs1 in
        let res_sequences = sequences bs in
        Sublist (sublist, res_sequences)
      else
        let sublist, bs = ascending b (STuple (b, a)) xs1 in
        let res_sequences = sequences bs in
        Sublist (sublist, res_sequences)
  | Cons (a, Nil) -> Singleton (a, End)
  | Nil -> End

and descending (a : int) (sublist : sublist) (bs : int_list) :
    sublist * int_list =
  match bs with
  | Cons (b, bs1) ->
      if a > b then descending b (SCons (b, sublist)) bs1
      else (sublist, Cons (b, bs1))
  | bs2 -> (sublist, bs2)

and ascending (a : int) (sublist : sublist) (bs : int_list) : sublist * int_list
    =
  match bs with
  | Cons (b, bs1) ->
      if a <= b then ascending b (SCons (b, sublist)) bs1
      else
        let reverse_sublist_res = reverse_sublist sublist in
        (reverse_sublist_res, Cons (b, bs1))
  | bs2 ->
      let reverse_sublist_res = reverse_sublist sublist in
      (reverse_sublist_res, bs2)

let rec to_list (c : sublist) (u : unit2) : int_list =
  match c with
  | SCons (a, cs) ->
      let cs_to_list = to_list cs u in
      Cons (a, cs_to_list)
  | STuple (a, b) -> ( match u with Unit2 (_, _) -> Cons (a, Cons (b, Nil)))

let rec merge_all (xs : partition) : int_list =
  match xs with
  | Sublist (x, End) -> to_list x (Unit2 (Pad, Pad))
  | Singleton (x, End) -> Cons (x, Nil)
  | xs2 ->
      let merge_pairs_res = merge_pairs xs2 in
      merge_all merge_pairs_res

and merge_pairs (xs : partition) : partition =
  match xs with
  | Sublist (a, Sublist (b, xs1)) ->
      let merge_res = merge a b (Unit2 (Pad, Pad)) in
      let merge_pairs_res = merge_pairs xs1 in
      Sublist (merge_res, merge_pairs_res)
  | Sublist (a, Singleton (b, xs1)) ->
      let merge_last_left_res = merge_last_left a b (Unit2 (Pad, Pad)) in
      let merge_pairs_res = merge_pairs xs1 in
      Sublist (merge_last_left_res, merge_pairs_res)
  | Singleton (a, Sublist (b, xs1)) ->
      let merge_last_right_res = merge_last_right a b (Unit2 (Pad, Pad)) in
      let merge_pairs_res = merge_pairs xs1 in
      Sublist (merge_last_right_res, merge_pairs_res)
  | Singleton (a, Singleton (b, xs1)) ->
      let sublist_arg1 = if a <= b then STuple (a, b) else STuple (b, a) in
      let merge_pairs_res = merge_pairs xs1 in
      Sublist (sublist_arg1, merge_pairs_res)
  | xs2 -> xs2

and merge (c1 : sublist) (c2 : sublist) (u : unit2) : sublist =
  match c1 with
  | SCons (a, cs1) -> (
      match c2 with
      | SCons (b, cs2) ->
          if a <= b then
            let merge_res = merge cs1 (SCons (b, cs2)) u in
            SCons (a, merge_res)
          else
            let merge_res = merge (SCons (a, cs1)) cs2 u in
            SCons (b, merge_res)
      | STuple (b, c) ->
          if a <= b then
            let merge_res = merge cs1 (STuple (b, c)) u in
            SCons (a, merge_res)
          else
            let merge_last_left_res = merge_last_left (SCons (a, cs1)) c u in
            SCons (b, merge_last_left_res))
  | STuple (a, b) -> (
      match c2 with
      | SCons (c, cs2) ->
          if a <= c then
            let merge_last_right_res = merge_last_right b (SCons (c, cs2)) u in
            SCons (a, merge_last_right_res)
          else
            let merge_res = merge (STuple (a, b)) cs2 u in
            SCons (c, merge_res)
      | STuple (c, d) ->
          if a <= c then
            let merge_right_res = merge_right b (Pair (c, d)) u in
            SCons (a, merge_right_res)
          else
            let merge_left_res = merge_left (Pair (a, b)) d u in
            SCons (c, merge_left_res))

and merge_last_right (a : int) (c2 : sublist) (u : unit2) : sublist =
  match c2 with
  | SCons (b, cs2) ->
      if a <= b then match u with Unit2 (_, _) -> SCons (a, SCons (b, cs2))
      else
        let merge_last_right_res = merge_last_right a cs2 u in
        SCons (b, merge_last_right_res)
  | STuple (b, c) -> merge_right a (Pair (b, c)) u

and merge_last_left (c2 : sublist) (d : int) (u : unit2) : sublist =
  match c2 with
  | SCons (a, cs2) -> (
      if a <= d then
        let merge_last_left_res = merge_last_left cs2 d u in
        SCons (a, merge_last_left_res)
      else match u with Unit2 (_, _) -> SCons (d, SCons (a, cs2)))
  | STuple (a, b) -> merge_left (Pair (a, b)) d u

and merge_right (a : int) (p : pair) (u : unit2) : sublist =
  match u with
  | Unit2 (_, _) -> (
      match p with
      | Pair (b, c) ->
          if a <= b then SCons (a, STuple (b, c))
          else
            let scons_arg2 = if a <= c then STuple (a, c) else STuple (c, a) in
            SCons (b, scons_arg2))

and merge_left (p : pair) (d : int) (u : unit2) : sublist =
  match u with
  | Unit2 (_, _) -> (
      match p with
      | Pair (a, b) ->
          if a <= d then
            let scons_arg2 = if b <= d then STuple (b, d) else STuple (d, b) in
            SCons (a, scons_arg2)
          else SCons (d, STuple (a, b)))

let rec rand_list (n : int) : int_list =
  if n > 0 then
    let pred_n = n - 1 in
    let rand_list_res = rand_list pred_n in
    Cons (n, rand_list_res)
  else Nil

let second_element (xs : int_list) (d : int) : int =
  match xs with Cons (_, Cons (x, _)) -> x | _ -> d

let rec test_iter (n : int) (iter : int) : int =
  if iter == 0 then 0
  else
    let xs = rand_list n in
    let sequences_res = sequences xs in
    let ys = merge_all sequences_res in
    let first_element_ys = second_element ys 0 in
    let pred_iter = iter - 1 in
    first_element_ys + test_iter n pred_iter

let test (n : int) : int = test_iter n n;;

print_int (test 10000)
