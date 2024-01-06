let%expect_test "simple custom type definition" = 
  let source_code = "
    type custom_simple_type = 
    | Constructor1
    | Constructor2
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Type Name: custom_simple_type
        Type Constructors:
            Type Constructor Name: Constructor1
            Type Constructor Name: Constructor2 |}]