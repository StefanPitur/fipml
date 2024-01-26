let%expect_test "custom type with function params" = 
  let source_code = "
    type custom_simple_type = 
    | Constructor1 of int -> int -> unit
    | Constructor2 of int option -> unit -> int option
    | Constructor3 of (int -> unit) option
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Type Name: custom_simple_type
        Type Constructors:
            Type Constructor Name: Constructor1
                Type Expr: (Int -> (Int -> Unit))
            Type Constructor Name: Constructor2
                Type Expr: (Int option -> (Unit -> Int option))
            Type Constructor Name: Constructor3
                Type Expr: (Int -> Unit) option |}]