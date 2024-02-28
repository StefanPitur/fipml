let%expect_test "function definition with borrowed parameters" =
  let source_code =
    "\n\
    \    fun function_name (^x : int) (^y : bool) : unit = begin\n\
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
        Param List:
            Type Expr: Int
            BorrowedParam: x
            Type Expr: Bool
            BorrowedParam: y
            Function Body Block
                Expr: Unit |}]
