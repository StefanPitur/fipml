let%expect_test "simple custom type definition" =
  let source_code =
    "\n    type custom_simple_type = \n    | Constructor1\n    | Constructor2\n  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Type Name: custom_simple_type
        Type Constructors:
            Type Constructor Name: Constructor1
            Type Constructor Name: Constructor2 |}]
;;
