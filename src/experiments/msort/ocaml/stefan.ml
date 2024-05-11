let rec f x = if x = 0 then 0 else x + f (x - 1)
and hello () = "Hello world!"
;;

hello ()
