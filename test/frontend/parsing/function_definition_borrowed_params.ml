let%expect_test "function definition with borrowed parameters" =
  let source_code =
    "\n\
    \    fun function_name ^(x : int @ shared) ^(y : bool @ shared) : unit @ \
     shared = {\n\
    \      ()\n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: function_name
        Mutually Recursive Group Id: 1
        Param Types:
            Type Expr: Int @ shared
            BorrowedParam: x
            Type Expr: Bool @ shared
            BorrowedParam: y
        Return Type: Unit @ shared
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
