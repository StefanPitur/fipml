let%expect_test "function definition with both borrowed and owned parameters" =
  let source_code =
    "\n\
    \    fun my_function ^(y1 : int) (x : custom_type) ^(y2 : unit) : [unit * \
     custom_type * int -> bool ] = { () }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_function
        Param Types:
            Type Expr: Int
            BorrowedParam: y1
            Type Expr: custom_type
            OwnedParam: x
            Type Expr: Unit
            BorrowedParam: y2
        Return Type: (Unit * custom_type * (Int -> Bool))
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
