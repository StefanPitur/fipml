let%expect_test "multiple type definitions" =
  let source_code =
    "\n\
    \    type custom_simple_type = \n\
    \    | SimpleConstructor1\n\n\
    \    type custom_complex_type =\n\
    \    | ComplexConstructor1 of 'a custom_simple_type @ unique\n\
    \    | ComplexConstructor2 of int @ 'u1 * bool @ 'u2 * unit @ shared * \
     custom_simple_type @ unique\n\
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
                Type Expr: (CustomArgPoly 'a) custom_simple_type @ unique
            Type Constructor Name: ComplexConstructor2
                Type Expr: Int @ 'u1
                Type Expr: Bool @ 'u2
                Type Expr: Unit @ shared
                Type Expr: custom_simple_type @ unique |}]
