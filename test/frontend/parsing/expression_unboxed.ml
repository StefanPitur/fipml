let%expect_test "expression: unboxed singleton/tuple" =
  let source_code_singleton = "{ variable_name }" in
  Pprint_parser_ast.pprint_parser_ast source_code_singleton;
  [%expect
    {|
    Program
        Main
            Expr: UnboxedSingleton
                Value: Var: variable_name |}];
  let source_code_tuple = "{ (1, false, Constructor ((), true), var_name) }" in
  Pprint_parser_ast.pprint_parser_ast source_code_tuple;
  [%expect
    {|
    Program
        Main
            Expr: UnboxedTuple
                Value: Int: 1
                Value: Bool: false
                Value: Constructor: Constructor
                    ConstructorArg
                        Value: Unit
                    ConstructorArg
                        Value: Bool: true
                Value: Var: var_name |}]
