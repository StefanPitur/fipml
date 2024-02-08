let%expect_test "complex type definition" =
  let source_code =
    "\n\
    \    type custom_complex_type = \n\
    \    | Constructor1\n\
    \    | Constructor2 of some_custom_type\n\
    \    | Constructor3 of int * some_custom_type\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: custom_complex_type
        Type Constructors:
            Type Constructor Name: Constructor1
            Type Constructor Name: Constructor2
                Type Expr: some_custom_type
            Type Constructor Name: Constructor3
                Type Expr: Int
                Type Expr: some_custom_type |}]
;;
