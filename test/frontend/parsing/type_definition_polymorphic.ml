let%expect_test "polymorphic custom type definition" =
  let source_code =
    "\n\
    \    type 'a simple_poly_type = \n\
    \    | Constructor1 of 'a\n\
    \    | Constructor2 of ('a option some_other_poly_time -> unit)\n\n\n\
    \    type 'a 'b complex_poly_type = \n\
    \    | ConstructorComplex1 of 'a * 'b\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: simple_poly_type
        Type Poly Params:
            Type Poly Param: 'a
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: 'a
            Type Constructor Name: Constructor2
                Type Expr: ((('a) option) some_other_poly_time -> Unit)
        Type Name: complex_poly_type
        Type Poly Params:
            Type Poly Param: 'a
            Type Poly Param: 'b
        Type Constructors:
            Type Constructor Name: ConstructorComplex1
                Type Expr: 'a
                Type Expr: 'b |}]
