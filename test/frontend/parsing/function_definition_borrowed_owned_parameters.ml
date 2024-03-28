let%expect_test "function definition with both borrowed and owned parameters" =
  let source_code =
    "\n\
    \    fun my_function ^(y1 : int) ^(y2 : unit) (x : custom_type) : unit = \
     begin () end\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: my_function
        Return Type: Unit
        Borrowed Param List:
            Type Expr: Int
            BorrowedParam: y1
            Type Expr: Unit
            BorrowedParam: y2
        Owned Param List:
            Type Expr: custom_type
            Param: x
            Function Body Block
                Expr: UnboxedSingleton
                    Value: Unit |}]

let%expect_test "function definition with both borrowed and owned parameters \
                 but in wrong order" =
  let source_code =
    "\n    fun my_function (x : unit) ^(y: int) : unit = begin () end\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {| ("Parsing.Lex_and_parse.ParserException(\"Syntax Error - File:  - Line: 2 - Column: 33\")") |}]
