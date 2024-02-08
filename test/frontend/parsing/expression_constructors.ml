let%expect_test "expression: constructor" =
  let source_code =
    "\n    ConstructorName(expr1, Some expr2, OtherConstructorName)\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Expr: Constructor: ConstructorName
            ConstructorArg
                Expr: Var: expr1
            ConstructorArg
                Expr: Option - Some
                    Expr: Var: expr2
            ConstructorArg
                Expr: Constructor: OtherConstructorName
                    () |}]
