open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: used type before actual definition" =
  let simple_custom_type_1 =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "custom_type_1",
        [
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C1",
              [ TECustom (mock_loc, [], Type_name.of_string "custom_type_2") ]
            );
        ] )
  in
  let simple_custom_type_2 =
    TType
      ( mock_loc,
        [],
        Type_name.of_string "custom_type_2",
        [ TTypeConstructor (mock_loc, Constructor_name.of_string "C2", []) ] )
  in
  match typecheck_type_defns [ simple_custom_type_1; simple_custom_type_2 ] with
  | Ok _ -> ()
  | Error err ->
      print_string (Error.to_string_hum err);
      [%expect
        {| ("Typing.Type_defns_env.TypeNotFound(\"File: mock - Line: 0 - Column: 1. Type custom_type_2 not defined\")") |}]
