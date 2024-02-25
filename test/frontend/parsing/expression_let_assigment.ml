let%expect_test "expression: let assigment" =
  let source_code = "begin let x = expr1 in expr2 end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: Let var: x =
                Expr: Var: expr1
                Expr: Var: expr2 |}]
