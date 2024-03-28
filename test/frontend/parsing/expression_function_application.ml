let%expect_test "expression: function application" =
  let source_code = "begin function_name ((parameter1, 0);) end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: FunCall
                Function Name: function_name
                Borrowed Args:
                Expr: UnboxedTuple
                    Value: Var: parameter1
                    Value: Int: 0
                Owned Args:
                () |}]
