let%expect_test "custom type with function params" =
  let source_code =
    "\n\
    \    type custom_simple_type = \n\
    \    | Constructor1 of ('a option my_custom_type @ shared -> (int @ \
     shared -> unit @ shared) @ shared) @ shared\n\
    \    | Constructor2 of ((int @ shared -> unit @ shared) @ shared -> int @ \
     shared) @ shared \n\
    \    | Constructor3 of (int @ shared -> unit @ shared) @ shared\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: custom_simple_type
        Type Poly Params:
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: ((CustomArgTyp (CustomArgPoly 'a) option) my_custom_type @ shared -> (Int @ shared -> Unit @ shared) @ shared) @ shared
            Type Constructor Name: Constructor2
                Type Expr: ((Int @ shared -> Unit @ shared) @ shared -> Int @ shared) @ shared
            Type Constructor Name: Constructor3
                Type Expr: (Int @ shared -> Unit @ shared) @ shared |}]
