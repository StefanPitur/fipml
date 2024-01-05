let%expect_test "recursive function definition" = 
  let source_code = "
    fun rec function_name (x : int) (^y : bool) = begin
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