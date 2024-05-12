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
                    Value: Var: x
                    Value: Var: y
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
                    Value: Var: y1
                    Value: Var: y2
                    Value: Var: y3
                    Value: Var: x |}]
