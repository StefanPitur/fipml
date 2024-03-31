let%expect_test "expression: FunApp" =
  let source_code = "{ ^e (x, y, z) }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: FunApp
                FunctionVar: e
                FunApp Args:
                    Expr: UnboxedSingleton
                        Value: Var: x
                    Expr: UnboxedSingleton
                        Value: Var: y
                    Expr: UnboxedSingleton
                        Value: Var: z |}]

let%expect_test "expression: FunCall" =
  let source_code = "{ func (y1, y2, y3, x) }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: FunCall
                Function Name: func
                FunCall Args:
                    Expr: UnboxedSingleton
                        Value: Var: y1
                    Expr: UnboxedSingleton
                        Value: Var: y2
                    Expr: UnboxedSingleton
                        Value: Var: y3
                    Expr: UnboxedSingleton
                        Value: Var: x |}]
