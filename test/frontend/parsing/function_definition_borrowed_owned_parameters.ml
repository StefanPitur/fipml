let%expect_test "function definition with both borrowed and owned parameters" =
  let source_code =
    "\n\
    \    fun my_function ^(y1 : int @ shared) (x : custom_type @ shared) ^(y2 \
     : unit @ shared) : (unit @ shared * custom_type @ shared * (int @ shared \
     -> bool @ shared) @ shared) @ shared = { () }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_function
        Mutually Recursive Group Id: 1
        Param Types:
            Type Expr: Int @ shared
            BorrowedParam: y1
            Type Expr: custom_type @ shared
            OwnedParam: x
            Type Expr: Unit @ shared
            BorrowedParam: y2
        Return Type: (Unit @ shared * custom_type @ shared * (Int @ shared -> Bool @ shared) @ shared) @ shared
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
