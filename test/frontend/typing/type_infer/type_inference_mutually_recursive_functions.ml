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
          [
            TParam
              ( TAttr
                  ( mock_loc,
                    TEInt mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ),
                Var_name.of_string "x",
                None );
          ],
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
          TAttr
            ( mock_loc,
              TEBool mock_loc,
              PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ) );
      TFun
        ( mock_loc,
          1,
          None,
          Function_name.of_string "is_odd",
          [
            TParam
              ( TAttr
                  ( mock_loc,
                    TEInt mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ),
                Var_name.of_string "x",
                None );
          ],
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
          TAttr
            ( mock_loc,
              TEBool mock_loc,
              PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ) );
      TFun
        ( mock_loc,
          2,
          None,
          Function_name.of_string "add",
          [
            TParam
              ( TAttr
                  ( mock_loc,
                    TEInt mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ),
                Var_name.of_string "x",
                None );
            TParam
              ( TAttr
                  ( mock_loc,
                    TEInt mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ),
                Var_name.of_string "y",
                None );
          ],
          BinaryOp
            ( mock_loc,
              BinOpPlus,
              UnboxedSingleton
                (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
              UnboxedSingleton
                (mock_loc, Variable (mock_loc, Var_name.of_string "y")) ),
          TAttr
            ( mock_loc,
              TEInt mock_loc,
              PolyUnique (mock_loc, Poly (mock_loc, "'u3")) ) );
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
  | Ok (typed_program, _, _, _, _) ->
      Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program;
      [%expect
        {|
        Typed Program - Bool @ u36
            Function Name: is_even
            Function Mutually Recursive Group Id - 1
            Return Type: Bool @ u11
            Param List:
                Type Expr: Int @ 'u1
                OwnedParam: x
            Function Body:
                Typed Expr: Let vars: (pred_x) : (Int @ u3) =
                    Typed Expr: - - Int @ u3
                        Typed Expr: UnboxedSingleton - Int @ shared
                            Value: Var: x - Int @ shared
                        Typed Expr: UnboxedSingleton - Int @ u4
                            Value: Int: 1 - Int @ u4
                Typed Expr: Let expr - Bool @ u11
                    Typed Expr: IfElse - Bool @ u11
                        Typed Expr: > - Bool @ u7
                            Typed Expr: UnboxedSingleton - Int @ shared
                                Value: Var: x - Int @ shared
                            Typed Expr: UnboxedSingleton - Int @ u8
                                Value: Int: 0 - Int @ u8
                    Then
                        Typed Expr: FunCall - Bool @ u11
                            Function Name: is_odd
                            FunctionArg
                                Value: Var: pred_x - Int @ u3
                    Else
                        Typed Expr: UnboxedSingleton - Bool @ u11
                            Value: Bool: true - Bool @ u11
            Function Name: is_odd
            Function Mutually Recursive Group Id - 1
            Return Type: Bool @ u22
            Param List:
                Type Expr: Int @ 'u1
                OwnedParam: x
            Function Body:
                Typed Expr: Let vars: (pred_x) : (Int @ u14) =
                    Typed Expr: - - Int @ u14
                        Typed Expr: UnboxedSingleton - Int @ shared
                            Value: Var: x - Int @ shared
                        Typed Expr: UnboxedSingleton - Int @ u15
                            Value: Int: 1 - Int @ u15
                Typed Expr: Let expr - Bool @ u22
                    Typed Expr: IfElse - Bool @ u22
                        Typed Expr: > - Bool @ u18
                            Typed Expr: UnboxedSingleton - Int @ shared
                                Value: Var: x - Int @ shared
                            Typed Expr: UnboxedSingleton - Int @ u19
                                Value: Int: 0 - Int @ u19
                    Then
                        Typed Expr: FunCall - Bool @ u22
                            Function Name: is_even
                            FunctionArg
                                Value: Var: pred_x - Int @ u14
                    Else
                        Typed Expr: UnboxedSingleton - Bool @ u22
                            Value: Bool: false - Bool @ u22
            Function Name: add
            Function Mutually Recursive Group Id - 2
            Return Type: Int @ u25
            Param List:
                Type Expr: Int @ 'u1
                OwnedParam: x
                Type Expr: Int @ 'u2
                OwnedParam: y
            Function Body:
                Typed Expr: + - Int @ u25
                    Typed Expr: UnboxedSingleton - Int @ u23
                        Value: Var: x - Int @ u23
                    Typed Expr: UnboxedSingleton - Int @ u24
                        Value: Var: y - Int @ u24
            Typed Main
            Typed Expr: Let vars: (sum_x_y) : (Int @ u31) =
                Typed Expr: FunCall - Int @ u31
                    Function Name: add
                    FunctionArg
                        Value: Int: 4 - Int @ u33
                    FunctionArg
                        Value: Int: 5 - Int @ u32
            Typed Expr: Let expr - Bool @ u36
                Typed Expr: FunCall - Bool @ u36
                    Function Name: is_even
                    FunctionArg
                        Value: Var: sum_x_y - Int @ u31 |}]
