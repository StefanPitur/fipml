open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: duplicated constructor definition in the \
                 same type" =
  let custom_type =
    TType
      ( mock_loc,
        [],
        [],
        [],
        Type_name.of_string "custom_type",
        [
          TTypeConstructor (mock_loc, Constructor_name.of_string "C1", []);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C1",
              [ TAttr (mock_loc, TEUnit mock_loc, Unique mock_loc) ] );
        ] )
  in
  match typecheck_type_defns [ custom_type ] with
  | Ok _ -> ()
  | Error err ->
      print_string (Error.to_string_hum err);
      [%expect
        {| ("Typing.Type_defns_env.ConstructorAlreadyExists(\"File: mock - Line: 0 - Column: 1. Duplicate definition of constructor C1\")") |}]

let%expect_test "typing type defn: duplicated constructor definition across \
                 different types" =
  let custom_type_1 =
    TType
      ( mock_loc,
        [],
        [],
        [],
        Type_name.of_string "custom_type_1",
        [ TTypeConstructor (mock_loc, Constructor_name.of_string "C1", []) ] )
  in
  let custom_type_2 =
    TType
      ( mock_loc,
        [],
        [],
        [],
        Type_name.of_string "custom_type_2",
        [
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C1",
              [ TAttr (mock_loc, TEUnit mock_loc, Unique mock_loc) ] );
        ] )
  in
  match typecheck_type_defns [ custom_type_1; custom_type_2 ] with
  | Ok _ -> ()
  | Error err ->
      print_string (Error.to_string_hum err);
      [%expect
        {| ("Typing.Type_defns_env.ConstructorAlreadyExists(\"File: mock - Line: 0 - Column: 1. Duplicate definition of constructor C1\")") |}]
