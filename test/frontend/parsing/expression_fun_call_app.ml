let%expect_test "expression: FunApp" =
  let source_code = "begin e (_; (x, y, z)) end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: FunApp
                FunctionVar: e
                Owned Args:
                Expr: UnboxedTuple
                    Value: Var: x
                    Value: Var: y
                    Value: Var: z |}]

let%expect_test "expression: FunCall" =
  let source_code = "begin func ((y1, y2, y3);x) end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
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
