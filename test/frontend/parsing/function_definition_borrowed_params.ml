let%expect_test "function definition with borrowed parameters" =
  let source_code =
    "\n\
    \    fun function_name ^(x : int) ^(y : bool) : unit = begin\n\
    \      ()\n\
    \    end\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: function_name
        Return Type: Unit
        Borrowed Param List:
            Type Expr: Int
            BorrowedParam: x
            Type Expr: Bool
            BorrowedParam: y
        Owned Param List:
            Void
        Function Body Block
            Expr: UnboxedSingleton
                Value: Unit |}]
