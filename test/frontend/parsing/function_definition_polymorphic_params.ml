let%expect_test "function definition with polymorphic parameters" =
  let source_code =
    "\n\
    \    fun my_poly_function (x : 'a option) (^y : int custom_type) : 'a \
     custom_type option = begin \n\
    \      () \n\
    \    end\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_poly_function
        Return Type: (('a) custom_type) option
        Param List:
            Type Expr: ('a) option
            Param: x
            Type Expr: (Int) custom_type
            BorrowedParam: y
            Function Body Block
                Expr: Unit |}]
