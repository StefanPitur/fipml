open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing
open Typing.Functions_env

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "Polymorphic Function Definition" =
  let functions_env =
    [
      FunctionEnvEntry
        ( 0,
          None,
          Function_name.of_string "poly_id",
          [ TEPoly (mock_loc, "'a"); TEInt mock_loc; TEPoly (mock_loc, "'b") ],
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
            [ Variable (mock_loc, Var_name.of_string "x"); Integer (mock_loc, 0); Boolean (mock_loc, true) ] ) )
  in
  match
    Type_infer.type_infer [] [] functions_env [] parsed_main ~verbose:true
  with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok typed_main_expr ->
      Pprint_typed_ast.pprint_typed_expr Fmt.stdout ~indent:"" typed_main_expr;
      [%expect {|
        Actual value:
        Value: Unit
        => Value Ty:
        TyUnit
        => Value Constraints:
        -------------------------

        Actual expr:
        Expr: UnboxedSingleton
            Value: Unit

        => Typing Context:
        => Expr Ty:
        TyUnit
        => Expr Constraints:
        -------------------------

        Actual value:
        Value: Bool: true
        => Value Ty:
        TyBool
        => Value Constraints:
        -------------------------

        Actual value:
        Value: Int: 0
        => Value Ty:
        TyInt
        => Value Constraints:
        -------------------------

        Actual value:
        Value: Var: x
        => Value Ty:
        TyUnit
        => Value Constraints:
        -------------------------

        Actual expr:
        Expr: FunCall
            Function Name: poly_id
            FunCall Args:
                Value: Var: x
                Value: Int: 0
                Value: Bool: true

        => Typing Context:
        x : TyPoly - for all (). TyUnit
        => Expr Ty:
        TyVar t2
        => Expr Constraints:
        (TyVar t1, TyUnit)
        (TyInt, TyInt)
        (TyVar t2, TyBool)
        -------------------------

        Actual expr:
        Expr: Let vars: (x) =
            Expr: UnboxedSingleton
                Value: Unit
            Expr: FunCall
                Function Name: poly_id
                FunCall Args:
                    Value: Var: x
                    Value: Int: 0
                    Value: Bool: true

        => Typing Context:
        x : TyPoly - for all (). TyUnit
        => Expr Ty:
        TyVar t2
        => Expr Constraints:
        (TyVar t1, TyUnit)
        (TyInt, TyInt)
        (TyVar t2, TyBool)
        -------------------------

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
