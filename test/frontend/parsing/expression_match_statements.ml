let%expect_test "expression: match" =
  let source_code =
    "\n\
    \    begin
    \      match x with\n\
    \      | _ -> begin () end\n\
    \      | y -> begin () end\n\
    \      | (y, z) -> begin () end\n\
    \      | Constructor1 -> begin () end\n\
    \      | Constructor2 (_) -> begin () end\n\
    \      | None -> begin () end\n\
    \      | Some _ -> begin () end\n\
    \      endmatch\n\
    \    end
    \  "
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
  let source_code =
    "begin match! x with\n    | _ -> begin () end\n    endmatch end"
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main Block
            Expr: DMatch
                DMatch Var: x
                PatternExpr
                    MatchedExpr: Underscore
                    PatternBlockExpr Block
                        Expr: Unit |}]
