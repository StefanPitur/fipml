let%expect_test "expression: fip function" =
  let source_code =
    "\n    fip fun my_fip_func ^(y : int) : unit = begin () end\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_fip_func
        Function Type - fip(0)
        Return Type: (Unit)
        Borrowed Param List:
            Type Expr: Int
            BorrowedParam: y
        Owned Param List:
            Void
        Function Body Block
            Expr: UnboxedSingleton
                Value: Unit |}]

let%expect_test "expression: fip function" =
  let source_code =
    "\n    fip(1) fun my_fip_func ^(y : int) : unit = begin () end\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Function Name: my_fip_func
          Function Type - fip(1)
          Return Type: (Unit)
          Borrowed Param List:
              Type Expr: Int
              BorrowedParam: y
          Owned Param List:
              Void
          Function Body Block
              Expr: UnboxedSingleton
                  Value: Unit |}]

let%expect_test "expression: fbip function" =
  let source_code =
    "\n    fbip fun my_fip_func ^(y : int) : unit = begin () end\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_fip_func
        Function Type - fbip(0)
        Return Type: (Unit)
        Borrowed Param List:
            Type Expr: Int
            BorrowedParam: y
        Owned Param List:
            Void
        Function Body Block
            Expr: UnboxedSingleton
                Value: Unit |}]

let%expect_test "expression: fip function" =
  let source_code =
    "\n    fbip(1) fun my_fip_func ^(y : int) : unit = begin () end\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Function Name: my_fip_func
          Function Type - fbip(1)
          Return Type: (Unit)
          Borrowed Param List:
              Type Expr: Int
              BorrowedParam: y
          Owned Param List:
              Void
          Function Body Block
              Expr: UnboxedSingleton
                  Value: Unit |}]
