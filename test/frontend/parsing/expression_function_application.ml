let%expect_test "expression: function application" = 
  let source_code = "
    function_name (parameter1, Some parameter2)
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: FunApp
            Function: function_name
            FunctionArg
                Expr: Var: parameter1
            FunctionArg
                Expr: Option - Some
                    Expr: Var: parameter2 |}]