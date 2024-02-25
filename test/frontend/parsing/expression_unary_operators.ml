open Ast.Ast_types

let%expect_test "expression: unary operators" =
  let unary_operators = [ UnOpFst; UnOpSnd; UnOpNeg; UnOpNot ] in
  let source_codes =
    List.map
      (fun unary_op -> "begin " ^ string_of_unary_op unary_op ^ " expr end")
      unary_operators
  in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Main Block
            Unary Op: fst
                Expr: Var: expr
    Program
        Main Block
            Unary Op: snd
                Expr: Var: expr
    Program
        Main Block
            Unary Op: -
                Expr: Var: expr
    Program
        Main Block
            Unary Op: !
                Expr: Var: expr |}]
