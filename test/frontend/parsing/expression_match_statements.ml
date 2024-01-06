let%expect_test "expression: match" = 
  let source_code = "
    match x with
    | _ -> begin () end
    | y -> begin () end
    | (y, z) -> begin () end
    | Constructor1 -> begin () end
    | Constructor2 (_) -> begin () end
    | None -> begin () end
    | Some _ -> begin () end
    endmatch
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: Match
            Match Var: x
            PatternExpr
                MatchedExpr: Underscore
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: Var - y
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: Tuple
                    MatchedExpr: Var - y
                    MatchedExpr: Var - z
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: Constructor - Constructor1
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: Constructor - Constructor2
                    MatchedExpr: Underscore
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: MOption - None
                PatternBlockExpr Block
                    Expr: Unit
            PatternExpr
                MatchedExpr: MOption - Some
                    MatchedExpr: Underscore
                PatternBlockExpr Block
                    Expr: Unit |}]

let%expect_test "expression: destructive match" = 
  let source_code = "
    match! x with
    | _ -> begin () end
    endmatch
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {|
    Program
        Expr: DMatch
            DMatch Var: x
            PatternExpr
                MatchedExpr: Underscore
                PatternBlockExpr Block
                    Expr: Unit |}]