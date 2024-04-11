let%expect_test "function definition with polymorphic parameters" =
  let source_code =
    "\n\
    \    fun my_poly_function (x : 'a option) ^(y : int custom_type) : 'a \
     custom_type option = { \n\
    \      () \n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_poly_function
        Mutually Recursive Group Id: 1
        Param Types:
            Type Expr: ('a) option
            OwnedParam: x
            Type Expr: (Int) custom_type
            BorrowedParam: y
        Return Type: (('a) custom_type) option
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
