open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing.Pprint_typed_ast

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "typing type defn: polymorphic custom type" =
  let option_type =
    TType
      ( mock_loc,
        [ TEPoly (mock_loc, Poly (mock_loc, "'t")) ],
        [ PolyUnique (mock_loc, Poly (mock_loc, "'u")) ],
        [],
        Type_name.of_string "option",
        [
          TTypeConstructor (mock_loc, Constructor_name.of_string "None", []);
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "Some",
              [
                TAttr
                  ( mock_loc,
                    TEPoly (mock_loc, Poly (mock_loc, "'t")),
                    PolyUnique (mock_loc, Poly (mock_loc, "'u")) );
              ] );
        ] )
  in
  let custom_type =
    TType
      ( mock_loc,
        [],
        [
          PolyUnique (mock_loc, Poly (mock_loc, "'u1"));
          PolyUnique (mock_loc, Poly (mock_loc, "'u2"));
        ],
        [ TPoly (Poly (mock_loc, "'a")); TPoly (Poly (mock_loc, "'b")) ],
        Type_name.of_string "custom_type",
        [
          TTypeConstructor
            ( mock_loc,
              Constructor_name.of_string "C",
              [
                TAttr
                  ( mock_loc,
                    TECustom
                      ( mock_loc,
                        [],
                        [
                          Shared mock_loc;
                          PolyUnique (mock_loc, Poly (mock_loc, "'u1"));
                        ],
                        [
                          TPoly (Poly (mock_loc, "'a"));
                          TPoly (Poly (mock_loc, "'b"));
                        ],
                        Type_name.of_string "custom_type" ),
                    PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
                TAttr
                  ( mock_loc,
                    TECustom
                      ( mock_loc,
                        [
                          TEArrow
                            ( mock_loc,
                              TAttr (mock_loc, TEUnit mock_loc, Shared mock_loc),
                              TPoly (Poly (mock_loc, "'b")) );
                        ],
                        [ PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ],
                        [],
                        Type_name.of_string "option" ),
                    PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
                TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
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
    ( ; 'u1, 'u2 ; 'a, 'b) custom_type
    ('t ; 'u ; ) option |}];
  Typing.Typecheck_type_defns.pprint_constructors_env Fmt.stdout
    constructors_env;
  [%expect
    {|
    Constructor Type : custom_type
        Type Constructor Name: C
            Type Expr: ( ; shared, 'u1 ; 'a, 'b) custom_type @ 'u1
            Type Expr: ((Unit @ shared -> 'b) ; 'u2 ; ) option @ 'u1
            Type Expr: Int @ unique
    Constructor Type : option
        Type Constructor Name: Some
            Type Expr: 't @ 'u
    Constructor Type : option
        Type Constructor Name: None |}];
  List.iter typed_type_defns ~f:(fun typed_type_defn ->
      pprint_typed_defn Fmt.stdout ~indent:"" typed_type_defn);
  [%expect
    {|
    Type Name: option
    Type Poly Params:
        Type Typ Poly Param: 't
        Type Unique Poly Param: 'u
    Type Constructors:
        Type Constructor Name: None
        Type Constructor Name: Some
            Type Expr: 't @ 'u
    Type Name: custom_type
    Type Poly Params:
        Type Unique Poly Param: 'u1
        Type Unique Poly Param: 'u2
        Type TypeExpr Poly Param: 'a
        Type TypeExpr Poly Param: 'b
    Type Constructors:
        Type Constructor Name: C
            Type Expr: ( ; shared, 'u1 ; 'a, 'b) custom_type @ 'u1
            Type Expr: ((Unit @ shared -> 'b) ; 'u2 ; ) option @ 'u1
            Type Expr: Int @ unique |}]
