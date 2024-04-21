open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "Type Inference on Mutually Recursive Functions" =
  let parsed_function_defns =
    [
      TFun
        ( mock_loc,
          1,
          None,
          Function_name.of_string "is_even",
          [ TParam (TEInt mock_loc, Var_name.of_string "x", None) ],
          Let
            ( mock_loc,
              [ Var_name.of_string "pred_x" ],
              BinaryOp
                ( mock_loc,
                  BinOpMinus,
                  UnboxedSingleton
                    (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
                  UnboxedSingleton (mock_loc, Integer (mock_loc, 1)) ),
              IfElse
                ( mock_loc,
                  BinaryOp
                    ( mock_loc,
                      BinOpGt,
                      UnboxedSingleton
                        (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
                      UnboxedSingleton (mock_loc, Integer (mock_loc, 0)) ),
                  FunCall
                    ( mock_loc,
                      Function_name.of_string "is_odd",
                      [ Variable (mock_loc, Var_name.of_string "pred_x") ] ),
                  UnboxedSingleton (mock_loc, Boolean (mock_loc, true)) ) ),
          TEBool mock_loc );
      TFun
        ( mock_loc,
          1,
          None,
          Function_name.of_string "is_odd",
          [ TParam (TEInt mock_loc, Var_name.of_string "x", None) ],
          Let
            ( mock_loc,
              [ Var_name.of_string "pred_x" ],
              BinaryOp
                ( mock_loc,
                  BinOpMinus,
                  UnboxedSingleton
                    (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
                  UnboxedSingleton (mock_loc, Integer (mock_loc, 1)) ),
              IfElse
                ( mock_loc,
                  BinaryOp
                    ( mock_loc,
                      BinOpGt,
                      UnboxedSingleton
                        (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
                      UnboxedSingleton (mock_loc, Integer (mock_loc, 0)) ),
                  FunCall
                    ( mock_loc,
                      Function_name.of_string "is_even",
                      [ Variable (mock_loc, Var_name.of_string "pred_x") ] ),
                  UnboxedSingleton (mock_loc, Boolean (mock_loc, false)) ) ),
          TEBool mock_loc );
      TFun
        ( mock_loc,
          2,
          None,
          Function_name.of_string "add",
          [
            TParam (TEInt mock_loc, Var_name.of_string "x", None);
            TParam (TEInt mock_loc, Var_name.of_string "y", None);
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
        [ Var_name.of_string "sum_x_y" ],
        FunCall
          ( mock_loc,
            Function_name.of_string "add",
            [ Integer (mock_loc, 4); Integer (mock_loc, 5) ] ),
        FunCall
          ( mock_loc,
            Function_name.of_string "is_even",
            [ Variable (mock_loc, Var_name.of_string "sum_x_y") ] ) )
  in
  let parsed_prog =
    TProg (mock_loc, [], parsed_function_defns, Some parsed_main)
  in
  match Typecheck_program.typecheck_program parsed_prog with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok (typed_program, _) ->
      Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program;
      [%expect
        {|
        Typed Program - Bool
            Function Name: is_even
            Function Mutually Recursive Group Id - 1
            Return Type: Bool
            Param List:
                Type Expr: Int
                OwnedParam: x
            Function Body:
                Typed Expr: Let vars: (pred_x) =
                    Typed Expr: - - Int
                        Typed Expr: UnboxedSingleton - Int
                            Value: Var: x - Int
                        Typed Expr: UnboxedSingleton - Int
                            Value: Int: 1 - Int
                Typed Expr: Let expr - Bool
                    Typed Expr: IfElse - Bool
                        Typed Expr: > - Bool
                            Typed Expr: UnboxedSingleton - Int
                                Value: Var: x - Int
                            Typed Expr: UnboxedSingleton - Int
                                Value: Int: 0 - Int
                    Then
                        Typed Expr: FunCall - Bool
                            Function Name: is_odd
                            FunctionArg
                                Value: Var: pred_x - Int
                    Else
                        Typed Expr: UnboxedSingleton - Bool
                            Value: Bool: true - Bool
            Function Name: is_odd
            Function Mutually Recursive Group Id - 1
            Return Type: Bool
            Param List:
                Type Expr: Int
                OwnedParam: x
            Function Body:
                Typed Expr: Let vars: (pred_x) =
                    Typed Expr: - - Int
                        Typed Expr: UnboxedSingleton - Int
                            Value: Var: x - Int
                        Typed Expr: UnboxedSingleton - Int
                            Value: Int: 1 - Int
                Typed Expr: Let expr - Bool
                    Typed Expr: IfElse - Bool
                        Typed Expr: > - Bool
                            Typed Expr: UnboxedSingleton - Int
                                Value: Var: x - Int
                            Typed Expr: UnboxedSingleton - Int
                                Value: Int: 0 - Int
                    Then
                        Typed Expr: FunCall - Bool
                            Function Name: is_even
                            FunctionArg
                                Value: Var: pred_x - Int
                    Else
                        Typed Expr: UnboxedSingleton - Bool
                            Value: Bool: false - Bool
            Function Name: add
            Function Mutually Recursive Group Id - 2
            Return Type: Int
            Param List:
                Type Expr: Int
                OwnedParam: x
                Type Expr: Int
                OwnedParam: y
            Function Body:
                Typed Expr: + - Int
                    Typed Expr: UnboxedSingleton - Int
                        Value: Var: x - Int
                    Typed Expr: UnboxedSingleton - Int
                        Value: Var: y - Int
            Typed Main
            Typed Expr: Let vars: (sum_x_y) =
                Typed Expr: FunCall - Int
                    Function Name: add
                    FunctionArg
                        Value: Int: 4 - Int
                    FunctionArg
                        Value: Int: 5 - Int
            Typed Expr: Let expr - Bool
                Typed Expr: FunCall - Bool
                    Function Name: is_even
                    FunctionArg
                        Value: Var: sum_x_y - Int |}]
