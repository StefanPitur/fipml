let source_code = "
  type my_first_type = 
    | C1
    | C2 of bool * int * my_second_type

  // some comment to be ignored
  /*Multi-line
    comment
  everywhere*/

  fun f4 (x : int) (^y : my_first_type) = begin
    ()
  end

  fun rec f5 (x1 : type1) (^y1 : type2) (x2 : int) (^y2 : int option) = begin
    (); fst (x1 + x2, ());
    match x1 with
    | x -> ()
    | y -> ()
    | C(x) -> ()
    | (x, y) -> true
    | Some (x, y) -> true
    | None -> false
    | 3 -> ()
    | _ -> false
    | Some _ -> true
    endmatch
  end

  let x = Some (x, y) in
  let y = None in 
  fn1 [(1, 2), x, y]
"
in
Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_string source_code);;