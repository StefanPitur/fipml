open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Pprint_typed_ast
open Typing.Typecheck_type_defns

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: simple custom type" =
  let simple_custom_type =
    TType
      ( mock_loc,
        [],
        [],
        [],
        Type_name.of_string "simple_custom_type",
        [
          TTypeConstructor (mock_loc, Constructor_name.of_string "C1", []);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C2",
              [
                TAttr (mock_loc, TEUnit mock_loc, Unique mock_loc);
                TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
                TAttr (mock_loc, TEBool mock_loc, Unique mock_loc);
                TAttr
                  ( mock_loc,
                    TEArrow
                      ( mock_loc,
                        TAttr (mock_loc, TEUnit mock_loc, Shared mock_loc),
                        TAttr (mock_loc, TEInt mock_loc, Shared mock_loc) ),
                    Unique mock_loc );
                TAttr
                  ( mock_loc,
                    TECustom
                      ( mock_loc,
                        [],
                        [],
                        [],
                        Type_name.of_string "simple_custom_type" ),
                    Unique mock_loc );
              ] );
        ] )
  in
  let types_env, constructors_env, typed_defns =
    Or_error.ok_exn (typecheck_type_defns [ simple_custom_type ])
  in
  pprint_types_env Fmt.stdout types_env;
  [%expect {| ( ;  ; ) simple_custom_type |}];
  pprint_constructors_env Fmt.stdout constructors_env;
  [%expect
    {|
    Constructor Type : simple_custom_type
        Type Constructor Name: C2
            Type Expr: Unit @ unique
            Type Expr: Int @ unique
            Type Expr: Bool @ unique
            Type Expr: (Unit @ shared -> Int @ shared) @ unique
            Type Expr: ( ;  ; ) simple_custom_type @ unique
    Constructor Type : simple_custom_type
        Type Constructor Name: C1 |}];
  List.iter typed_defns ~f:(fun typed_defn ->
      pprint_typed_defn Fmt.stdout ~indent:"" typed_defn);
  [%expect
    {|
    Type Name: simple_custom_type
    Type Poly Params:
    Type Constructors:
        Type Constructor Name: C1
        Type Constructor Name: C2
            Type Expr: Unit @ unique
            Type Expr: Int @ unique
            Type Expr: Bool @ unique
            Type Expr: (Unit @ shared -> Int @ shared) @ unique
            Type Expr: ( ;  ; ) simple_custom_type @ unique |}]
