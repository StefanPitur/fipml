open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: simple custom type" =
  let simple_custom_type =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "simple_custom_type",
        [
          TTypeConstructor (mock_loc, Constructor_name.of_string "C1", []);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C2",
              [
                TEUnit mock_loc;
                TEInt mock_loc;
                TEBool mock_loc;
                TEArrow (mock_loc, TEUnit mock_loc, TEInt mock_loc);
                TECustom (mock_loc, [], Type_name.of_string "simple_custom_type");
              ] );
        ] )
  in
  let types_env, constructors_env, _ =
    Or_error.ok_exn (typecheck_type_defns [ simple_custom_type ])
  in
  pprint_types_env Fmt.stdout types_env;
  [%expect {| () simple_custom_type |}];
  pprint_constructors_env Fmt.stdout constructors_env;
  [%expect
    {|
    Constructor Type : simple_custom_type
        Type Constructor Name: C2
            Type Expr: Unit
            Type Expr: Int
            Type Expr: Bool
            Type Expr: (Unit -> Int)
            Type Expr: simple_custom_type
    Constructor Type : simple_custom_type
        Type Constructor Name: C1 |}]
