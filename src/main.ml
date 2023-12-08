let source_code = "
  type my_first_type = 
    | C1
    | C2 of bool * int * my_second_type

  // some comment to be ignored
  /*Multi-line
    comment
  everywhere*/

  let fn1 = ()
  let fn2 = begin
    ()
  end
  let f3 x y z = ()
  let f4 (x : int) (^y : my_first_type) = ()
"
in
Frontend.Lex_and_parse.parse_source_code (Lexing.from_string source_code);;