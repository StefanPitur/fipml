open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "Polymorphic Function Definition" =
  let parsed_function_defns =
    [
      TFun
        ( mock_loc,
          1,
          None,
          Function_name.of_string "poly_id",
          [
            TParam (TEPoly (mock_loc, "'a"), Var_name.of_string "x", None);
            TParam (TEInt mock_loc, Var_name.of_string "y", None);
            TParam (TEPoly (mock_loc, "'b"), Var_name.of_string "z", None);
          ],
          UnboxedSingleton
            (mock_loc, Variable (mock_loc, Var_name.of_string "z")),
          TEPoly (mock_loc, "'b") );
    ]
  in
  let parsed_main =
    Let
      ( mock_loc,
        [ Var_name.of_string "x" ],
        UnboxedSingleton (mock_loc, Unit mock_loc),
        FunCall
          ( mock_loc,
            Function_name.of_string "poly_id",
            [
              Variable (mock_loc, Var_name.of_string "x");
              Integer (mock_loc, 0);
              Boolean (mock_loc, true);
            ] ) )
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
            Function Name: poly_id
            Function Mutually Recursive Group Id - 1
            Return Type: t2
            Param List:
                Type Expr: 'a
                OwnedParam: x
                Type Expr: Int
                OwnedParam: y
                Type Expr: 'b
                OwnedParam: z
            Function Body:
                Typed Expr: UnboxedSingleton - t2
                    Value: Var: z - t2
            Typed Main
            Typed Expr: Let vars: (x) =
                Typed Expr: UnboxedSingleton - Unit
                    Value: Unit - Unit
            Typed Expr: Let expr - Bool
                Typed Expr: FunCall - Bool
                    Function Name: poly_id
                    FunctionArg
                        Value: Var: x - Unit
                    FunctionArg
                        Value: Int: 0 - Int
                    FunctionArg
                        Value: Bool: true - Bool |}]
