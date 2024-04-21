open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: duplicated custom type definition" =
  let custom_type =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "custom_type",
        [ TTypeConstructor (mock_loc, Constructor_name.of_string "C1", []) ] )
  in
  let duplicated_custom_type =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "custom_type",
        [
          TTypeConstructor
            (mock_loc, Constructor_name.of_string "C2", [ TEUnit mock_loc ]);
        ] )
  in
  match typecheck_type_defns [ custom_type; duplicated_custom_type ] with
  | Ok _ -> ()
  | Error err ->
      print_string (Error.to_string_hum err);
      [%expect
        {| ("Typing.Type_defns_env.TypeAlreadyExists(\"File: mock - Line: 0 - Column: 1. Duplicate definition of type () custom_type\")") |}]
