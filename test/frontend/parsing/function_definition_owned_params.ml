let%expect_test "function definition with owned parameters" =
  let source_code =
    "\n\
    \    fun function_name (x : int @ shared) (y : bool @ shared) (z : unit @ \
     shared) (t : custom_type @ shared) : (unit @ shared * int @ shared * \
     custom_type @ shared * bool @ shared * ((int @ shared -> int @ shared) @ \
     shared -> int @ shared) @ shared) @ shared = {\n\
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
            OwnedParam: x
            Type Expr: Bool @ shared
            OwnedParam: y
            Type Expr: Unit @ shared
            OwnedParam: z
            Type Expr: custom_type @ shared
            OwnedParam: t
        Return Type: (Unit @ shared * Int @ shared * custom_type @ shared * Bool @ shared * ((Int @ shared -> Int @ shared) @ shared -> Int @ shared) @ shared) @ shared
        Function Body Expr
            Expr: UnboxedSingleton
                Value: Unit |}]
