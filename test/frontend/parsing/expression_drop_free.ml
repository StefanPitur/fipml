let%expect_test "expression: drop" =
  let source_code = "begin drop x end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Main Block
            Expr: Drop - x |}]

let%expect_test "expression: free" =
  let source_code = "begin free k end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: Free
                Value: Var: k |}]
