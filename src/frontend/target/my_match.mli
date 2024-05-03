type some_type = Smth of unit

type mylist =
  | Nil of { mutable hd : unit }
  | Cons of { mutable hd : int; mutable tl : mylist }

val main : mylist -> mylist
