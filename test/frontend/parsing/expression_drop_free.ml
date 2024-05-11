let%expect_test "expression: drop" =
  let source_code = "{ drop x; () }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: Drop x - Expr:
                Expr: UnboxedSingleton
                    Value: Unit |}]

let%expect_test "expression: free" =
  let source_code = "{ free 1; () }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: Free 1
            Free Expr
                Expr: UnboxedSingleton
                    Value: Unit |}]
