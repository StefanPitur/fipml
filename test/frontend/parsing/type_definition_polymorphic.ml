let%expect_test "polymorphic custom type definition" =
  let source_code =
    "\n\
    \    type ( @ 'u @ 'a) simple_poly_type = \n\
    \    | Constructor1 of ( ; unique ; ) custom_type @ 'u\n\
    \    | Constructor2 of ((; ; ( ; ; 'a) option @ 'u) some_other_poly_time @ \
     shared -> unit @ unique) @ unique\n\n\n\
    \    type ('t1 @ 'u1 @ ) complex_poly_type = \n\
    \    | ConstructorComplex1 of 't1 @ 'u1 * (int; shared; 't1 @ 'u1) \
     custom_type @ 'u2\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: simple_poly_type
        Type Poly Params:
            Type Unique Poly Param: 'u
            Type Type Expr Poly Param: 'a
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: ( ; unique ; ) custom_type @ 'u
            Type Constructor Name: Constructor2
                Type Expr: (( ;  ; ( ;  ; 'a) option @ 'u) some_other_poly_time @ shared -> Unit @ unique) @ unique
        Type Name: complex_poly_type
        Type Poly Params:
            Type Typ Poly Param: 't1
            Type Unique Poly Param: 'u1
        Type Constructors:
            Type Constructor Name: ConstructorComplex1
                Type Expr: 't1 @ 'u1
                Type Expr: (Int ; shared ; 't1 @ 'u1) custom_type @ 'u2 |}]
