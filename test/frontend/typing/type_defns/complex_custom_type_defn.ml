open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: complex custom type" =
  let simple_custom_type =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "simple_custom_type",
        [ TTypeConstructor (mock_loc, Constructor_name.of_string "SC1", []) ] )
  in
  let complex_custom_type =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "complex_custom_type",
        [
          TTypeConstructor
            (mock_loc, Constructor_name.of_string "CC1", [ TEUnit mock_loc ]);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "CC2",
              [
                TECustom (mock_loc, [], Type_name.of_string "complex_custom_type");
                TECustom (mock_loc, [], Type_name.of_string "simple_custom_type");
              ] );
        ] )
  in
  let types_env, constructors_env, _ =
    Or_error.ok_exn
      (typecheck_type_defns [ simple_custom_type; complex_custom_type ])
  in
  pprint_types_env Fmt.stdout types_env;
  [%expect {|
    complex_custom_type
    simple_custom_type |}];
  pprint_constructors_env Fmt.stdout constructors_env;
  [%expect
    {|
    Constructor Type : complex_custom_type
        Type Constructor Name: CC2
            Type Expr: complex_custom_type
            Type Expr: simple_custom_type
    Constructor Type : complex_custom_type
        Type Constructor Name: CC1
            Type Expr: Unit
    Constructor Type : simple_custom_type
        Type Constructor Name: SC1 |}]
