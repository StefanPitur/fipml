let%expect_test "expression: let assigment" =
  let source_code_singleton_let = "{ let (x, y, z) = expr1 in expr2 }" in
  Pprint_parser_ast.pprint_parser_ast source_code_singleton_let;
  [%expect
    {|
    Program
        Main
            Expr: Let vars: (x, y, z) =
                Expr: UnboxedSingleton
                    Value: Var: expr1
                Expr: UnboxedSingleton
                    Value: Var: expr2 |}];
  let source_code = "{ let (x1, x2) = expr1 in expr2 }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: Let vars: (x1, x2) =
                Expr: UnboxedSingleton
                    Value: Var: expr1
                Expr: UnboxedSingleton
                    Value: Var: expr2 |}]
