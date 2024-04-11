open Ast.Ast_types

let%expect_test "expression: unary operators" =
  let unary_operators = [ UnOpNeg; UnOpNot ] in
  let source_codes =
    List.map
      (fun unary_op -> "{ " ^ string_of_unary_op unary_op ^ " var }")
      unary_operators
  in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Main
            Unary Op: -
                Expr: UnboxedSingleton
                    Value: Var: var
    Program
        Main
            Unary Op: !
                Expr: UnboxedSingleton
                    Value: Var: var |}]
