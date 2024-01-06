let%expect_test "complex type definition" = 
  let source_code = "
    type custom_complex_type = 
    | Constructor1
    | Constructor2 of some_custom_type
    | Constructor3 of int * some_custom_type
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Type Name: custom_complex_type
        Type Constructors:
            Type Constructor Name: Constructor1
            Type Constructor Name: Constructor2
                Type Expr: some_custom_type
            Type Constructor Name: Constructor3
                Type Expr: Int
                Type Expr: some_custom_type |}]