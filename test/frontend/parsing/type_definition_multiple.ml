let%expect_test "multiple type definitions" =
  let source_code = "
    type custom_simple_type = 
    | SimpleConstructor1

    type custom_complex_type =
    | ComplexConstructor1 of custom_simple_type
    | ComplexConstructor2 of int * bool * unit * custom_simple_type
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Type Name: custom_simple_type
        Type Constructors:
            Type Constructor Name: SimpleConstructor1
        Type Name: custom_complex_type
        Type Constructors:
            Type Constructor Name: ComplexConstructor1
                Type Expr: custom_simple_type
            Type Constructor Name: ComplexConstructor2
                Type Expr: Int
                Type Expr: Bool
                Type Expr: Unit
                Type Expr: custom_simple_type |}]