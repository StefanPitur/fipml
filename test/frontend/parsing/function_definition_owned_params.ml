let%expect_test "function definition with owned parameters" =
  let source_code =
    "\n\
    \    fun function_name (x : int) (y : bool) (z : unit) (t : custom_type \
     option) : unit = begin\n\
    \      ()\n\
    \    end\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Function Name: function_name
        Return Type: Unit
        Param List:
            Type Expr: Int
            Param: x
            Type Expr: Bool
            Param: y
            Type Expr: Unit
            Param: z
            Type Expr: custom_type option
            Param: t
            Function Body Block
                Expr: Unit |}]
