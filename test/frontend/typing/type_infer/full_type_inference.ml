open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "Full Type Checking" =
  let parsed_typed_defns =
    [
      TType
        ( mock_loc,
          [],
          Type_name.of_string "custom_type",
          [
            TTypeConstructor
              (mock_loc, Constructor_name.of_string "C1", [ TEInt mock_loc ]);
            TTypeConstructor
              ( mock_loc,
                Constructor_name.of_string "C2",
                [
                  TEBool mock_loc;
                  TECustom (mock_loc, [], Type_name.of_string "custom_type");
                ] );
          ] );
    ]
  in
  let parsed_function_defns =
    [
      TFun
        ( mock_loc,
          1,
          Some (Fip 0),
          Function_name.of_string "add",
          [
            TParam (TEInt mock_loc, Var_name.of_string "x", None);
            TParam (TEInt mock_loc, Var_name.of_string "y", Some Borrowed);
          ],
          BinaryOp
            ( mock_loc,
              BinOpPlus,
              UnboxedSingleton
                (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
              UnboxedSingleton
                (mock_loc, Variable (mock_loc, Var_name.of_string "y")) ),
          TEInt mock_loc );
    ]
  in
  let parsed_main =
    Let
      ( mock_loc,
        [ Var_name.of_string "x" ],
        UnboxedSingleton (mock_loc, Integer (mock_loc, 5)),
        Let
          ( mock_loc,
            [ Var_name.of_string "y" ],
            UnboxedSingleton
              ( mock_loc,
                Constructor
                  ( mock_loc,
                    Constructor_name.of_string "C1",
                    [ Integer (mock_loc, 10) ] ) ),
            Let
              ( mock_loc,
                [ Var_name.of_string "z1"; Var_name.of_string "z2" ],
                Match
                  ( mock_loc,
                    Var_name.of_string "y",
                    [
                      MPattern
                        ( mock_loc,
                          MUnderscore mock_loc,
                          Let
                            ( mock_loc,
                              [ Var_name.of_string "res" ],
                              FunCall
                                ( mock_loc,
                                  Function_name.of_string "add",
                                  [
                                    Integer (mock_loc, 0);
                                    Variable (mock_loc, Var_name.of_string "x");
                                  ] ),
                              UnboxedTuple
                                ( mock_loc,
                                  [
                                    Variable (mock_loc, Var_name.of_string "res");
                                    Boolean (mock_loc, true);
                                  ] ) ) );
                      MPattern
                        ( mock_loc,
                          MConstructor
                            ( mock_loc,
                              Constructor_name.of_string "C1",
                              [ MVariable (mock_loc, Var_name.of_string "t") ]
                            ),
                          Let
                            ( mock_loc,
                              [ Var_name.of_string "res" ],
                              FunCall
                                ( mock_loc,
                                  Function_name.of_string "add",
                                  [
                                    Variable (mock_loc, Var_name.of_string "t");
                                    Variable (mock_loc, Var_name.of_string "x");
                                  ] ),
                              UnboxedTuple
                                ( mock_loc,
                                  [
                                    Variable (mock_loc, Var_name.of_string "res");
                                    Boolean (mock_loc, true);
                                  ] ) ) );
                      MPattern
                        ( mock_loc,
                          MConstructor
                            ( mock_loc,
                              Constructor_name.of_string "C2",
                              [
                                MVariable (mock_loc, Var_name.of_string "t");
                                MConstructor
                                  ( mock_loc,
                                    Constructor_name.of_string "C1",
                                    [ MUnderscore mock_loc ] );
                              ] ),
                          Let
                            ( mock_loc,
                              [ Var_name.of_string "res" ],
                              FunCall
                                ( mock_loc,
                                  Function_name.of_string "add",
                                  [
                                    Integer (mock_loc, 2);
                                    Variable (mock_loc, Var_name.of_string "x");
                                  ] ),
                              UnboxedTuple
                                ( mock_loc,
                                  [
                                    Variable (mock_loc, Var_name.of_string "res");
                                    Variable (mock_loc, Var_name.of_string "t");
                                  ] ) ) );
                    ] ),
                IfElse
                  ( mock_loc,
                    UnboxedSingleton
                      (mock_loc, Variable (mock_loc, Var_name.of_string "z2")),
                    UnboxedSingleton
                      (mock_loc, Variable (mock_loc, Var_name.of_string "z1")),
                    UnboxedSingleton
                      (mock_loc, Variable (mock_loc, Var_name.of_string "x")) )
              ) ) )
  in
  let parsed_prog =
    TProg (mock_loc, parsed_typed_defns, parsed_function_defns, Some parsed_main)
  in
  match Typecheck_program.typecheck_program parsed_prog with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok typed_program ->
      Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program;
      [%expect
        {|
          Typed Program - Int
              Type Name: custom_type
              Type Poly Params:
              Type Constructors:
                  Type Constructor Name: C1
                      Type Expr: Int
                  Type Constructor Name: C2
                      Type Expr: Bool
                      Type Expr: custom_type
              Fip(0) Function Name: add
              Function Mutually Recursive Group Id - 1
              Return Type: Int
              Param List:
                  Type Expr: Int
                  OwnedParam: x
                  Type Expr: Int
                  BorrowedParam: y
              Function Body:
                  Typed Expr: + - Int
                      Typed Expr: UnboxedSingleton - Int
                          Value: Var: x - Int
                      Typed Expr: UnboxedSingleton - Int
                          Value: Var: y - Int
              Typed Main
              Typed Expr: Let vars: (x) =
                  Typed Expr: UnboxedSingleton - Int
                      Value: Int: 5 - Int
              Typed Expr: Let expr - Int
                  Typed Expr: Let vars: (y) =
                      Typed Expr: UnboxedSingleton - custom_type
                          Value: Constructor: C1 - custom_type
                                  ConstructorArg
                                  Value: Int: 10 - Int
                  Typed Expr: Let expr - Int
                      Typed Expr: Let vars: (z1, z2) =
                          Typed Expr: Match - (Int * Bool)
                              Match Var: y
                              PatternExpr - (Int * Bool)
                                  Typed MatchedExpr - custom_type : Underscore
                              PatternMatchExpr
                                  Typed Expr: Let vars: (res) =
                                      Typed Expr: FunCall - Int
                                          Function Name: add
                                          FunctionArg
                                              Value: Int: 0 - Int
                                          FunctionArg
                                              Value: Var: x - Int
                                  Typed Expr: Let expr - (Int * Bool)
                                      Typed Expr: UnboxedTuple - (Int * Bool)
                                          Value: Var: res - Int
                                          Value: Bool: true - Bool
                              PatternExpr - (Int * Bool)
                                  Typed MatchedExpr - custom_type : C1
                                      Typed MatchedExpr - Int : Var t
                              PatternMatchExpr
                                  Typed Expr: Let vars: (res) =
                                      Typed Expr: FunCall - Int
                                          Function Name: add
                                          FunctionArg
                                              Value: Var: t - Int
                                          FunctionArg
                                              Value: Var: x - Int
                                  Typed Expr: Let expr - (Int * Bool)
                                      Typed Expr: UnboxedTuple - (Int * Bool)
                                          Value: Var: res - Int
                                          Value: Bool: true - Bool
                              PatternExpr - (Int * Bool)
                                  Typed MatchedExpr - custom_type : C2
                                      Typed MatchedExpr - Bool : Var t
                                      Typed MatchedExpr - custom_type : C1
                                          Typed MatchedExpr - Int : Underscore
                              PatternMatchExpr
                                  Typed Expr: Let vars: (res) =
                                      Typed Expr: FunCall - Int
                                          Function Name: add
                                          FunctionArg
                                              Value: Int: 2 - Int
                                          FunctionArg
                                              Value: Var: x - Int
                                  Typed Expr: Let expr - (Int * Bool)
                                      Typed Expr: UnboxedTuple - (Int * Bool)
                                          Value: Var: res - Int
                                          Value: Var: t - Bool
                      Typed Expr: Let expr - Int
                          Typed Expr: IfElse - Int
                              Typed Expr: UnboxedSingleton - Bool
                                  Value: Var: z2 - Bool
                          Then
                              Typed Expr: UnboxedSingleton - Int
                                  Value: Var: z1 - Int
                          Else
                              Typed Expr: UnboxedSingleton - Int
                                  Value: Var: x - Int |}]

let%expect_test "Full Type Checking - Let-Polymorphism constructors and match \
                 expressions" =
  let parsed_typed_defns =
    [
      TType
        ( mock_loc,
          [ TEPoly (mock_loc, "'a") ],
          Type_name.of_string "option",
          [
            TTypeConstructor (mock_loc, Constructor_name.of_string "None", []);
            TTypeConstructor
              ( mock_loc,
                Constructor_name.of_string "Some",
                [ TEPoly (mock_loc, "'a") ] );
          ] );
    ]
  in
  let parsed_main =
    Let
      ( mock_loc,
        [ Var_name.of_string "x" ],
        UnboxedSingleton
          ( mock_loc,
            Constructor (mock_loc, Constructor_name.of_string "None", []) ),
        Match
          ( mock_loc,
            Var_name.of_string "x",
            [
              MPattern
                ( mock_loc,
                  MConstructor (mock_loc, Constructor_name.of_string "None", []),
                  UnboxedSingleton (mock_loc, Integer (mock_loc, 0)) );
              MPattern
                ( mock_loc,
                  MConstructor
                    ( mock_loc,
                      Constructor_name.of_string "Some",
                      [ MVariable (mock_loc, Var_name.of_string "y") ] ),
                  BinaryOp
                    ( mock_loc,
                      BinOpPlus,
                      UnboxedSingleton
                        (mock_loc, Variable (mock_loc, Var_name.of_string "y")),
                      UnboxedSingleton (mock_loc, Integer (mock_loc, 1)) ) );
              MPattern
                ( mock_loc,
                  MConstructor
                    ( mock_loc,
                      Constructor_name.of_string "Some",
                      [ MUnderscore mock_loc ] ),
                  UnboxedSingleton (mock_loc, Integer (mock_loc, 3)) );
            ] ) )
  in
  let parsed_prog =
    TProg (mock_loc, parsed_typed_defns, [], Some parsed_main)
  in
  match Typecheck_program.typecheck_program parsed_prog with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok typed_program ->
      Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program;
      [%expect
        {|
        Typed Program - Int
            Type Name: option
            Type Poly Params:
                Type Poly Param: 'a
            Type Constructors:
                Type Constructor Name: None
                Type Constructor Name: Some
                    Type Expr: 'a
            Typed Main
            Typed Expr: Let vars: (x) =
                Typed Expr: UnboxedSingleton - (t9) option
                    Value: Constructor: None - (t9) option
                        ()
            Typed Expr: Let expr - Int
                Typed Expr: Match - Int
                    Match Var: x
                    PatternExpr - Int
                        Typed MatchedExpr - (Int) option : None
                    PatternMatchExpr
                        Typed Expr: UnboxedSingleton - Int
                            Value: Int: 0 - Int
                    PatternExpr - Int
                        Typed MatchedExpr - (Int) option : Some
                            Typed MatchedExpr - Int : Var y
                    PatternMatchExpr
                        Typed Expr: + - Int
                            Typed Expr: UnboxedSingleton - Int
                                Value: Var: y - Int
                            Typed Expr: UnboxedSingleton - Int
                                Value: Int: 1 - Int
                    PatternExpr - Int
                        Typed MatchedExpr - (Int) option : Some
                            Typed MatchedExpr - Int : Underscore
                    PatternMatchExpr
                        Typed Expr: UnboxedSingleton - Int
                            Value: Int: 3 - Int |}]
