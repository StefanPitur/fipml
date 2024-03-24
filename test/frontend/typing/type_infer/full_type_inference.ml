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
                [ TEInt mock_loc; TEInt mock_loc ] );
          ] );
    ]
  in
  let parsed_function_defns =
    [
      TFun
        ( mock_loc,
          Function_name.of_string "add_y",
          [
            TParam
              ( TECustom (mock_loc, [], Type_name.of_string "custom_type"),
                Var_name.of_string "x",
                None );
            TParam (TEInt mock_loc, Var_name.of_string "y", None);
          ],
          Block
            ( mock_loc,
              [
                Match
                  ( mock_loc,
                    Var_name.of_string "x",
                    [
                      MPattern
                        ( mock_loc,
                          MConstructor
                            ( mock_loc,
                              Constructor_name.of_string "C1",
                              [ MVariable (mock_loc, Var_name.of_string "x") ]
                            ),
                          Block
                            ( mock_loc,
                              [
                                Unit mock_loc;
                                IfElse
                                  ( mock_loc,
                                    BinaryOp
                                      ( mock_loc,
                                        BinOpGt,
                                        Variable
                                          (mock_loc, Var_name.of_string "y"),
                                        Integer (mock_loc, 0) ),
                                    Block
                                      ( mock_loc,
                                        [
                                          BinaryOp
                                            ( mock_loc,
                                              BinOpMod,
                                              Variable
                                                ( mock_loc,
                                                  Var_name.of_string "y" ),
                                              Integer (mock_loc, 5) );
                                        ] ),
                                    Block
                                      ( mock_loc,
                                        [
                                          BinaryOp
                                            ( mock_loc,
                                              BinOpDiv,
                                              BinaryOp
                                                ( mock_loc,
                                                  BinOpPlus,
                                                  Variable
                                                    ( mock_loc,
                                                      Var_name.of_string "x" ),
                                                  Variable
                                                    ( mock_loc,
                                                      Var_name.of_string "y" )
                                                ),
                                              Integer (mock_loc, 3) );
                                        ] ) );
                              ] ) );
                      MPattern
                        ( mock_loc,
                          MConstructor
                            ( mock_loc,
                              Constructor_name.of_string "C2",
                              [
                                MVariable (mock_loc, Var_name.of_string "x");
                                MVariable (mock_loc, Var_name.of_string "x2");
                              ] ),
                          Block
                            ( mock_loc,
                              [
                                BinaryOp
                                  ( mock_loc,
                                    BinOpPlus,
                                    Variable (mock_loc, Var_name.of_string "y"),
                                    FunApp
                                      ( mock_loc,
                                        Function_name.of_string "add_y",
                                        [
                                          Constructor
                                            ( mock_loc,
                                              Constructor_name.of_string "C1",
                                              [
                                                Variable
                                                  ( mock_loc,
                                                    Var_name.of_string "x" );
                                              ] );
                                          Variable
                                            (mock_loc, Var_name.of_string "x2");
                                        ] ) );
                              ] ) );
                    ] );
              ] ),
          TEInt mock_loc );
    ]
  in
  let parsed_main =
    Block
      ( mock_loc,
        [
          Unit mock_loc;
          FunApp
            ( mock_loc,
              Function_name.of_string "add_y",
              [
                Constructor
                  ( mock_loc,
                    Constructor_name.of_string "C1",
                    [ Integer (mock_loc, 2) ] );
                Integer (mock_loc, 5);
              ] );
        ] )
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
                  Type Constructor Name: C2
                      Type Expr: Int
                      Type Expr: Int
                  Type Constructor Name: C1
                      Type Expr: Int
              Function Name: add_y
              Return Type: Int
              Param List:
                  Type Expr: custom_type
                  Param: x
                  Type Expr: Int
                  Param: y
                  Function Body Block - Int
                      Typed Expr: Match - Int
                          Match Var: x
                          PatternExpr - Int
                              Typed MatchedExpr - custom_type : C2
                                  Typed MatchedExpr - Int : Var x2
                                  Typed MatchedExpr - Int : Var x
                              PatternBlockExpr Block - Int
                                  Typed Expr: + - Int
                                      Typed Expr: Var y - Int
                                      Typed Expr: FunApp - Int
                                          Function: add_y
                                          FunctionArg
                                              Typed Expr: Constructor C1 - custom_type
                                                  ConstructorArg
                                                      Typed Expr: Var x - Int
                                          FunctionArg
                                              Typed Expr: Var x2 - Int
                          PatternExpr - Int
                              Typed MatchedExpr - custom_type : C1
                                  Typed MatchedExpr - Int : Var x
                              PatternBlockExpr Block - Int
                                  Typed Expr: Unit - Unit
                                  Typed Expr: IfElse - Int
                                      Typed Expr: > - Bool
                                          Typed Expr: Var y - Int
                                          Typed Expr: Int: 0 - Int
                                      Then Block - Int
                                          Typed Expr: % - Int
                                              Typed Expr: Var y - Int
                                              Typed Expr: Int: 5 - Int
                                      Else Block - Int
                                          Typed Expr: / - Int
                                              Typed Expr: + - Int
                                                  Typed Expr: Var x - Int
                                                  Typed Expr: Var y - Int
                                              Typed Expr: Int: 3 - Int
              Typed Main Block - Int
                  Typed Expr: Unit - Unit
                  Typed Expr: FunApp - Int
                      Function: add_y
                      FunctionArg
                          Typed Expr: Constructor C1 - custom_type
                              ConstructorArg
                                  Typed Expr: Int: 2 - Int
                      FunctionArg
                          Typed Expr: Int: 5 - Int |}]
