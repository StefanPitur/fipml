let%expect_test "expression: let assigment" =
  let source_code = "let x = expr1 in expr2" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Expr: Let var: x =
            Expr: Var: expr1
            Expr: Var: expr2 |}]
;;
