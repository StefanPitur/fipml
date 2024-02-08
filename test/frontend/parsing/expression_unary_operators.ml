open Ast.Ast_types

let%expect_test "expression: unary operators" =
  let unary_operators = [ UnOpFst; UnOpSnd; UnOpNeg; UnOpNot ] in
  let source_codes =
    List.map (fun unary_op -> string_of_unary_op unary_op ^ " expr") unary_operators
  in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Unary Op: fst
            Expr: Var: expr
    Program
        Unary Op: snd
            Expr: Var: expr
    Program
        Unary Op: -
            Expr: Var: expr
    Program
        Unary Op: !
            Expr: Var: expr |}]
;;
