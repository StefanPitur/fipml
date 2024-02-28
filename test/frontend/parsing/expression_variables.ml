let%expect_test "expression: variable" =
  let source_code = "begin variable_name end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: Var: variable_name |}]
