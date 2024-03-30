let%expect_test "expression: If" =
  let source_code = "{ if cond_var then { then_expr } endif }" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: If
                Expr: UnboxedSingleton
                    Value: Var: cond_var
                Then
                Expr: UnboxedSingleton
                    Value: Var: then_expr |}]

let%expect_test "expression: IfElse" =
  let source_code =
    "\n\
    \    {\n\
    \      if cond_var then\n\
    \        {\n\
    \          then_expr\n\
    \        }\n\
    \      else\n\
    \        {\n\
    \          else_expr\n\
    \        }\n\
    \      endif\n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: IfElse
                Expr: UnboxedSingleton
                    Value: Var: cond_var
                Then
                Expr: UnboxedSingleton
                    Value: Var: then_expr
                Else
                Expr: UnboxedSingleton
                    Value: Var: else_expr |}]
