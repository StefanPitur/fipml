let%expect_test "function definition with owned parameters" =
  let source_code =
    "\n\
    \    fun function_name (x : int) (y : bool) (z : unit) (t : custom_type ) \
     : [unit * int * custom_type * bool * (int -> int) -> int] = {\n\
    \      ()\n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: function_name
        Param Types:
            Type Expr: Int
            OwnedParam: x
            Type Expr: Bool
            OwnedParam: y
            Type Expr: Unit
            OwnedParam: z
            Type Expr: custom_type
            OwnedParam: t
        Return Type: (Unit * Int * custom_type * Bool * ((Int -> Int) -> Int))
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
