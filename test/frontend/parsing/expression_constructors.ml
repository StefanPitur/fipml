let%expect_test "expression: constructor" =
  let source_code =
    "begin ConstructorName(expr1, expr2, OtherConstructorName) end"
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Main Block
              Expr: Constructor: ConstructorName
                  ConstructorArg
                      Expr: Var: expr1
                  ConstructorArg
                      Expr: Var: expr2
                  ConstructorArg
                      Expr: Constructor: OtherConstructorName
                          () |}]
