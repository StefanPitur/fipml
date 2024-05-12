let%expect_test "expression: match" =
  let source_code =
    "\n\
    \        {\n\
    \            match x with\n\
    \            | _ -> { true }\n\
    \            | Constructor1 -> { 0 }\n\
    \            | Constructor2 (_, x, y) -> { (x, y) }\n\
    \            endmatch\n\
    \        }\n\
    \     "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
       Program
           Main
               Expr: Match
                   Match Var: x
                   Pattern
                       MatchedExpr: Underscore
                       PatternExpr
                       Expr: UnboxedSingleton
                           Value: Bool: true
                   Pattern
                       MatchedExpr: Constructor - Constructor1
                       PatternExpr
                       Expr: UnboxedSingleton
                           Value: Int: 0
                   Pattern
                       MatchedExpr: Constructor - Constructor2
                           MatchedExpr: Underscore
                           MatchedExpr: Var - x
                           MatchedExpr: Var - y
                       PatternExpr
                       Expr: UnboxedTuple
                           Value: Var: x
                           Value: Var: y |}]

let%expect_test "expression: match nested constructors" =
  let source_code =
    "\n\
    \    {\n\
    \        match x with\n\
    \        | ComplexConstructor (_, x, Atom, ComplexConstructor2 (_)) -> { \
     () }\n\
    \        endmatch\n\
    \    }\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
    Program
        Main
            Expr: Match
                Match Var: x
                Pattern
                    MatchedExpr: Constructor - ComplexConstructor
                        MatchedExpr: Underscore
                        MatchedExpr: Var - x
                        MatchedExpr: Constructor - Atom
                        MatchedExpr: Constructor - ComplexConstructor2
                            MatchedExpr: Underscore
                    PatternExpr
                    Expr: UnboxedSingleton
                        Value: Unit |}]
