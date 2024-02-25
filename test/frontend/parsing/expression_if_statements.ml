let%expect_test "expression: If" =
  let source_code = "begin if cond_var then begin then_expr end endif end" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: If
                Expr: Var: cond_var
                Then Block
                    Expr: Var: then_expr |}]

let%expect_test "expression: IfElse" =
  let source_code =
    "\n\
    \    begin\n\
    \      if cond_var then\n\
    \        begin\n\
    \          then_expr\n\
    \        end\n\
    \      else\n\
    \        begin\n\
    \          else_expr\n\
    \        end\n\
    \      endif\n\
    \    end\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: IfElse
                Expr: Var: cond_var
                Then Block
                    Expr: Var: then_expr
                Else Block
                    Expr: Var: else_expr |}]
