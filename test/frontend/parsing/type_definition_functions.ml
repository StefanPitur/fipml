let%expect_test "custom type with function params" =
  let source_code =
    "\n\
    \    type custom_simple_type = \n\
    \    | Constructor1 of my_custom_type -> int -> unit\n\
    \    | Constructor2 of int -> unit -> int\n\
    \    | Constructor3 of (int -> unit)\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: custom_simple_type
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: (my_custom_type -> (Int -> Unit))
            Type Constructor Name: Constructor2
                Type Expr: (Int -> (Unit -> Int))
            Type Constructor Name: Constructor3
                Type Expr: (Int -> Unit) |}]
