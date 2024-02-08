let%expect_test "expression: value" =
  let source_codes = [ "None"; "()"; "1"; "true"; "false" ] in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Expr: Option - None
    Program
        Expr: Unit
    Program
        Expr: Int: 1
    Program
        Expr: Bool: true
    Program
        Expr: Bool: false |}]
;;
