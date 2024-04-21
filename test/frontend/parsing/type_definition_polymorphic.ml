let%expect_test "polymorphic custom type definition" =
  let source_code =
    "\n\
    \    type ('a, 'u) simple_poly_type = \n\
    \    | Constructor1 of 'a\n\
    \    | Constructor2 of (('a option @ 'u) some_other_poly_time @ shared -> \
     unit @ unique) @ unique\n\n\n\
    \    type ('u1, 'u2) complex_poly_type = \n\
    \    | ConstructorComplex1 of int @ 'u1 * custom_type @ 'u2\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: simple_poly_type
        Type Poly Params:
            Type Poly Param: 'a
            Type Poly Param: 'u
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: 'a
            Type Constructor Name: Constructor2
                Type Expr: ((CustomArgTypeExpr (CustomArgPoly 'a) option @ 'u) some_other_poly_time @ shared -> Unit @ unique) @ unique
        Type Name: complex_poly_type
        Type Poly Params:
            Type Poly Param: 'u1
            Type Poly Param: 'u2
        Type Constructors:
            Type Constructor Name: ConstructorComplex1
                Type Expr: Int @ 'u1
                Type Expr: custom_type @ 'u2 |}]
