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
      (fun binary_op -> "{ var1 " ^ string_of_binary_op binary_op ^ "var2 }")
      binary_operators
  in
  List.iter Pprint_parser_ast.pprint_parser_ast source_codes;
  [%expect
    {|
    Program
        Main
            Binary Op: +
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: -
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: *
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: /
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: %
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: <
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: >
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: <=
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: >=
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: ==
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: !=
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: &&
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2
    Program
        Main
            Binary Op: ||
                LeftExpr
                Expr: UnboxedSingleton
                    Value: Var: var1
                RightExpr
                Expr: UnboxedSingleton
                    Value: Var: var2 |}]
