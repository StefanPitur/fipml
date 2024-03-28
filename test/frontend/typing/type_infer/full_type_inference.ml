(* open Ast.Ast_types
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
             Type_name.of_string "custom_type",
             [
               TTypeConstructor
                 (mock_loc, Constructor_name.of_string "C", [ TEInt mock_loc ]);
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
                 ( TEOption
                     ( mock_loc,
                       TECustom (mock_loc, Type_name.of_string "custom_type") ),
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
                             MOption (mock_loc, None),
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
                                             UnOp
                                               ( mock_loc,
                                                 UnOpNeg,
                                                 Variable
                                                   ( mock_loc,
                                                     Var_name.of_string "y" ) );
                                           ] ) );
                                 ] ) );
                         MPattern
                           ( mock_loc,
                             MOption
                               ( mock_loc,
                                 Some
                                   (MConstructor
                                      ( mock_loc,
                                        Constructor_name.of_string "C",
                                        [
                                          MVariable
                                            (mock_loc, Var_name.of_string "x");
                                        ] )) ),
                             Block
                               ( mock_loc,
                                 [
                                   FunApp
                                     ( mock_loc,
                                       Function_name.of_string "add_y",
                                       [
                                         Option (mock_loc, None);
                                         Variable (mock_loc, Var_name.of_string "x");
                                       ] );
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
             Let
               ( mock_loc,
                 Var_name.of_string "x",
                 Option (mock_loc, None),
                 FunApp
                   ( mock_loc,
                     Function_name.of_string "add_y",
                     [
                       Option
                         ( mock_loc,
                           Some
                             (Constructor
                                ( mock_loc,
                                  Constructor_name.of_string "C",
                                  [ Integer (mock_loc, 2) ] )) );
                       Integer (mock_loc, 5);
                     ] ) );
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
           Type Constructors:
               Type Constructor Name: C
                   Type Expr: Int
           Function Name: add_y
           Return Type: Int
           Param List:
               Type Expr: custom_type option
               Param: x
               Type Expr: Int
               Param: y
               Function Body Block - Int
                   Typed Expr: Match - Int
                       Match Var: x
                       PatternExpr - Int
                           Typed MatchedExpr - custom_type option : Option Some
                               Typed MatchedExpr - custom_type : C
                                   Typed MatchedExpr - Int : Var x
                           PatternBlockExpr Block - Int
                               Typed Expr: FunApp - Int
                                   Function: add_y
                                   FunctionArg
                                       Typed Expr: Option None - custom_type option
                                   FunctionArg
                                       Typed Expr: Var x - Int
                       PatternExpr - Int
                           Typed MatchedExpr - custom_type option : Option None
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
                                       Typed Expr: - - Int
                                           Typed Expr: Var y - Int
           Typed Main Block - Int
               Typed Expr: Let var x - _undefined
                   Typed Expr: Option None - _undefined
               Typed Expr: Let expr - Int
                   Typed Expr: FunApp - Int
                       Function: add_y
                       FunctionArg
                           Typed Expr: Option Some - custom_type option
                               Typed Expr: Constructor C - custom_type
                                   ConstructorArg
                                       Typed Expr: Int: 2 - Int
                       FunctionArg
                           Typed Expr: Int: 5 - Int |}] *)
