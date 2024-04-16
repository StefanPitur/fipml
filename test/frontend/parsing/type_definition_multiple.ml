let%expect_test "multiple type definitions" =
  let source_code =
    "\n\
    \    type custom_simple_type = \n\
    \    | SimpleConstructor1\n\n\
    \    type custom_complex_type =\n\
    \    | ComplexConstructor1 of custom_simple_type\n\
    \    | ComplexConstructor2 of int * bool * unit * custom_simple_type\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: custom_simple_type
        Type Poly Params:
        Type Constructors:
            Type Constructor Name: SimpleConstructor1
        Type Name: custom_complex_type
        Type Poly Params:
        Type Constructors:
            Type Constructor Name: ComplexConstructor1
                Type Expr: custom_simple_type
            Type Constructor Name: ComplexConstructor2
                Type Expr: Int
                Type Expr: Bool
                Type Expr: Unit
                Type Expr: custom_simple_type |}]
