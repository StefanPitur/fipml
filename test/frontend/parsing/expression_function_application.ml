let%expect_test "expression: function application" =
  let source_code =
    "begin function_name (parameter1, Constructor(parameter2)) end"
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Main Block
              Expr: FunApp
                  Function: function_name
                  FunctionArg
                      Expr: Var: parameter1
                  FunctionArg
                      Expr: Constructor: Constructor
                          ConstructorArg
                              Expr: Var: parameter2 |}]
