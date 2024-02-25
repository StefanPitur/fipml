open Ast.Ast_types

let%expect_test "expression: binary operators" =
  let binary_operators =
    [
      BinOpPlus;
      BinOpMinus;
      BinOpMult;
      BinOpDiv;
      BinOpMod;
      BinOpLt;
      BinOpGt;
      BinOpLeq;
      BinOpGeq;
      BinOpEq;
      BinOpNeq;
      BinOpAnd;
      BinOpOr;
    ]
  in
  let source_codes =
    List.map
      (fun binary_op -> "begin expr1 " ^ string_of_binary_op binary_op ^ "expr2 end")
      binary_operators
  in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Main Block
            Binary Op: +
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: -
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: *
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: /
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: %
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: <
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: >
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: <=
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: >=
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: ==
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: !=
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: &&
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2
    Program
        Main Block
            Binary Op: ||
                LeftExpr
                Expr: Var: expr1
                RightExpr
                Expr: Var: expr2 |}]
