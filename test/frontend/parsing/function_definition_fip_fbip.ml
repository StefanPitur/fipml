let%expect_test "expression: fip function" =
  let source_code =
    "\n    fip fun my_fip_func ^(y : int @ shared) : unit @ shared = { () }\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_fip_func
        Mutually Recursive Group Id: 1
        Function Type - fip(0)
        Param Types:
            Type Expr: Int @ shared
            BorrowedParam: y
        Return Type: Unit @ shared
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]

let%expect_test "expression: fip function" =
  let source_code =
    "\n\
    \    fip(1) fun my_fip_func ^(y : int @ shared) : unit @ shared = { () }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Function Name: my_fip_func
          Mutually Recursive Group Id: 2
          Function Type - fip(1)
          Param Types:
              Type Expr: Int @ shared
              BorrowedParam: y
          Return Type: Unit @ shared
          Function Body Expr
              Expr: UnboxedSingleton
                  Value: Unit |}]

let%expect_test "expression: fbip function" =
  let source_code =
    "\n\
    \    fbip fun my_fip_func ^(y : int @ shared) : unit @ shared = { () }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_fip_func
        Mutually Recursive Group Id: 3
        Function Type - fbip(0)
        Param Types:
            Type Expr: Int @ shared
            BorrowedParam: y
        Return Type: Unit @ shared
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]

let%expect_test "expression: fip function" =
  let source_code =
    "\n\
    \    fbip(1) fun my_fip_func ^(y : int @ shared) : unit @ shared = { () }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Function Name: my_fip_func
          Mutually Recursive Group Id: 4
          Function Type - fbip(1)
          Param Types:
              Type Expr: Int @ shared
              BorrowedParam: y
          Return Type: Unit @ shared
          Function Body Expr
              Expr: UnboxedSingleton
                  Value: Unit |}]
