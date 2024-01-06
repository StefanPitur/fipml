let%expect_test "expression: If" = 
  let source_code = "if cond_var then begin then_expr end endif" in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: If
            Expr: Var: cond_var
            Then Block
                Expr: Var: then_expr |}]

let%expect_test "expression: IfElse" = 
  let source_code = "
    if cond_var then
      begin
        then_expr
      end
    else
      begin
        else_expr
      end
    endif
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: IfElse
            Expr: Var: cond_var
            Then Block
                Expr: Var: then_expr
            Else Block
                Expr: Var: else_expr |}]