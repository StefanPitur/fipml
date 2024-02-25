open Core

let%expect_test "expression: value" =
  let values = [ "None"; "()"; "1"; "true"; "false" ] in
  let source_codes =
    List.map values ~f:(fun value -> "begin " ^ value ^ " end")
  in
  List.iter source_codes ~f:Pprint_parser_ast.pprint_parser_ast;
  [%expect
    {|
    Program
        Main Block
            Expr: Option - None
    Program
        Main Block
            Expr: Unit
    Program
        Main Block
            Expr: Int: 1
    Program
        Main Block
            Expr: Bool: true
    Program
        Main Block
            Expr: Bool: false |}]
