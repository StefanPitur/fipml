(* type 'a mlist =
     | Nil
     | Cons of { mutable hd: 'a; mutable tl: 'a mlist }

   let rec reverse_acc xs acc =
     match xs with
     |  Cons r ->
         let xx = r.tl in
         r.tl <- acc;
         reverse_acc xx (Cons r)
     | Nil -> acc

   let lst = Cons { hd = 1; tl = Cons { hd = 2; tl = Cons { hd = 3; tl = Nil } } };;
   let my_ref = ref 0;; *)

let x = ref 0 in
x := !x + 1;
!x
