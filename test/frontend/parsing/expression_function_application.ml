let%expect_test "expression: function application" =
  let source_code = "{ function_name (parameter1, 0) }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: FunCall
                Function Name: function_name
                FunCall Args:
                    Expr: UnboxedSingleton
                        Value: Var: parameter1
                    Expr: UnboxedSingleton
                        Value: Int: 0 |}]
