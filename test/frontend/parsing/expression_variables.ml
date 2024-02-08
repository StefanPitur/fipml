let%expect_test "expression: variable" =
  let source_code = "variable_name" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: Var: variable_name |}]
;;
