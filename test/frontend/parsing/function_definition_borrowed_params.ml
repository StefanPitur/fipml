let%expect_test "function definition with borrowed parameters" =
  let source_code =
    "\n\
    \    fun function_name ^(x : int) ^(y : bool) : unit = {\n\
    \      ()\n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: function_name
        Param Types:
            Type Expr: Int
            BorrowedParam: x
            Type Expr: Bool
            BorrowedParam: y
        Return Type: (Unit)
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
