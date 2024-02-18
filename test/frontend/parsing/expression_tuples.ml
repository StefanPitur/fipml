let%expect_test "expression: tuples" =
  let source_code = "(expr1, expr2)" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Expr: Tuple
            Fst
            Expr: Var: expr1
            Snd
            Expr: Var: expr2 |}]
