let%expect_test "expression: match" =
  let source_code =
    "\n\
    \        begin\n\
    \            match x with\n\
    \            | _ -> begin () end\n\
    \            | y -> begin ((), true) end\n\
    \            | Constructor1 -> begin (); 0 end\n\
    \            | Constructor2 (_, x, y) -> begin (); (x, y) end\n\
    \            endmatch\n\
    \        end\n\
    \     "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
       Program
           Main Block
               Expr: Match
                   Match Var: x
                   PatternExpr
                       MatchedExpr: Underscore
                       PatternBlockExpr Block
                           Expr: UnboxedSingleton
                               Value: Unit
                   PatternExpr
                       MatchedExpr: Var - y
                       PatternBlockExpr Block
                           Expr: UnboxedTuple
                               Value: Unit
                               Value: Bool: true
                   PatternExpr
                       MatchedExpr: Constructor - Constructor1
                       PatternBlockExpr Block
                           Expr: UnboxedSingleton
                               Value: Unit
                           Expr: UnboxedSingleton
                               Value: Int: 0
                   PatternExpr
                       MatchedExpr: Constructor - Constructor2
                           MatchedExpr: Underscore
                           MatchedExpr: Var - x
                           MatchedExpr: Var - y
                       PatternBlockExpr Block
                           Expr: UnboxedSingleton
                               Value: Unit
                           Expr: UnboxedTuple
                               Value: Var: x
                               Value: Var: y |}]
