let%expect_test "function definition with borrowed parameters" = 
  let source_code = "
    fun function_name (^x : int) (^y : bool) = begin
      ()
    end
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Function Name: function_name
        Param List:
            Type Expr: Int
            Param: x
            Type Expr: Bool
            BorrowedParam: y
            Function Body Block
                Expr: Unit |}]