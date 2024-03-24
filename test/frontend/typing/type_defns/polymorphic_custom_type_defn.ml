open Ast.Ast_types
open Core
open Parsing.Parser_ast

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: polymorphic custom type" =
  let option_type =
    TType
      ( mock_loc,
        [ TEPoly (mock_loc, "'a") ],
        Type_name.of_string "option",
        [
          TTypeConstructor (mock_loc, Constructor_name.of_string "None", []);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "Some",
              [
                TECustom
                  ( mock_loc,
                    [ TEPoly (mock_loc, "'a") ],
                    Type_name.of_string "option" );
              ] );
        ] )
  in
  let custom_type =
    TType
      ( mock_loc,
        [ TEPoly (mock_loc, "'a"); TEPoly (mock_loc, "'b") ],
        Type_name.of_string "custom_type",
        [
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C",
              [
                TECustom
                  ( mock_loc,
                    [ TEPoly (mock_loc, "'a"); TEPoly (mock_loc, "'b") ],
                    Type_name.of_string "custom_type" );
                TECustom
                  ( mock_loc,
                    [
                      TEArrow
                        (mock_loc, TEUnit mock_loc, TEPoly (mock_loc, "'a"));
                    ],
                    Type_name.of_string "option" );
                TEInt mock_loc;
              ] );
        ] )
  in
  let types_env, constructors_env, typed_type_defns =
    Or_error.ok_exn
      (Typing.Typecheck_type_defns.typecheck_type_defns
         [ option_type; custom_type ])
  in
  Typing.Typecheck_type_defns.pprint_types_env Fmt.stdout types_env;
  [%expect {|
    ('a, 'b) custom_type
    ('a) option |}];
  Typing.Typecheck_type_defns.pprint_constructors_env Fmt.stdout
    constructors_env;
  [%expect
    {|
    Constructor Type : custom_type
        Type Constructor Name: C
            Type Expr: ('a, 'b) custom_type
            Type Expr: ((Unit -> 'a)) option
            Type Expr: Int
    Constructor Type : option
        Type Constructor Name: Some
            Type Expr: ('a) option
    Constructor Type : option
        Type Constructor Name: None |}];
  Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout
    (Typing.Typed_ast.TProg (typed_type_defns, TEUnit mock_loc, [], None));
  [%expect
    {|
    Typed Program - Unit
        Type Name: custom_type
        Type Poly Params:
            Type Poly Param: 'a
            Type Poly Param: 'b
        Type Constructors:
            Type Constructor Name: C
                Type Expr: ('a, 'b) custom_type
                Type Expr: ((Unit -> 'a)) option
                Type Expr: Int
        Type Name: option
        Type Poly Params:
            Type Poly Param: 'a
        Type Constructors:
            Type Constructor Name: Some
                Type Expr: ('a) option
            Type Constructor Name: None |}]
