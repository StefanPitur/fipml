let%expect_test "function definition with polymorphic parameters" =
  let source_code =
    "\n\
    \    fun my_poly_function (x : 'a option @ unique) ^(y : (int @ unique) \
     custom_type @ unique) : ('a custom_type @ unique) option @ unique = { \n\
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
            Type Expr: (CustomArgPoly 'a) option @ unique
            OwnedParam: x
            Type Expr: (CustomArgTypeExpr Int @ unique) custom_type @ unique
            BorrowedParam: y
        Return Type: (CustomArgTypeExpr (CustomArgPoly 'a) custom_type @ unique) option @ unique
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
