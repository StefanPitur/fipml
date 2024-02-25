let%expect_test "expression: tuples" =
  let source_code = "begin (expr1, expr2) end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: Tuple
                Fst
                Expr: Var: expr1
                Snd
                Expr: Var: expr2 |}]
