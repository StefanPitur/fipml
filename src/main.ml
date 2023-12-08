let source_code = "
  type my_first_type = 
    | C1
    | C2 of bool * int * my_second_type

  // some comment to be ignored
  /*Multi-line
    comment
  everywhere*/

  fun fn1 = ()
  fun fn2 = begin
    ()
  end
  fun rec f3 x y z = ()
  fun f4 (x : int) (^y : my_first_type) = ()

  fun rec f5 x1 ^y1 (x2 : int) (^y2 : my_type) = begin
    (); fst (x1 + x2, ());
    match x1 with
    | x -> ()
    | y -> ()
    | C(x) -> ()
    | (x, y) -> true
    | 3 -> ()
    | _ -> false
    endmatch
  end
"
in
Frontend.Lex_and_parse.parse_source_code (Lexing.from_string source_code);;