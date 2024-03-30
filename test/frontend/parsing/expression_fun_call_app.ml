let%expect_test "expression: FunApp" =
  let source_code = "{ e (_; (x, y, z)) }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: FunApp
                FunctionVar: e
                Owned Args:
                Expr: UnboxedTuple
                    Value: Var: x
                    Value: Var: y
                    Value: Var: z |}]

let%expect_test "expression: FunCall" =
  let source_code = "{ func ((y1, y2, y3); x) }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: FunCall
                Function Name: func
                Borrowed Args:
                Expr: UnboxedTuple
                    Value: Var: y1
                    Value: Var: y2
                    Value: Var: y3
                Owned Args:
                Expr: UnboxedSingleton
                    Value: Var: x |}]
