let%expect_test "recursive function definition" =
  let source_code =
    "\n\
    \    fun rec function_name (x : int) (^y : bool) : unit = begin\n\
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
            Param: x
            Type Expr: Bool
            BorrowedParam: y
            Function Body Block
                Expr: Unit |}]
