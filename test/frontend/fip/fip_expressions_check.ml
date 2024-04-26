open Ast.Ast_types
open Core
open Typing.Fip_rules_check
open Typing.Fbip_rules_check
open Typing.Borrowed_context
open Typing.Pprint_fip_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let functions_env : Functions_env.functions_env =
  [
    FunctionEnvEntry
      ( 1,
        Some (Fip 0),
        Function_name.of_string "add_fun",
        [ TAttr (mock_loc, TEInt mock_loc, Unique mock_loc) ],
        [ None ],
        TAttr
          ( mock_loc,
            TEInt mock_loc,
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ) );
    FunctionEnvEntry
      ( 2,
        Some (Fip 0),
        Function_name.of_string "apply_fun",
        [
          TAttr
            ( mock_loc,
              TEArrow
                ( mock_loc,
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc) ),
              Shared mock_loc );
          TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
        ],
        [ Some Borrowed; None ],
        TAttr
          ( mock_loc,
            TEInt mock_loc,
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ) );
  ]

let%expect_test "FIP rules for expressions : UnboxedSingleton" =
  let expr =
    Typed_ast.UnboxedSingleton
      ( mock_loc,
        TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
        Typed_ast.Variable
          ( mock_loc,
            TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
            Var_name.of_string "x" ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - []
    Reuse

    Fip Expr: UnboxedSingleton
        Borrowed - []
        Owned - []
        Reuse

        Fip Value: Variable - x |}]

let%expect_test "FIP rules for expressions : UnboxedTuple" =
  let expr =
    Typed_ast.UnboxedTuple
      ( mock_loc,
        TAttr
          ( mock_loc,
            TETuple
              ( mock_loc,
                [
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
                ] ),
            Shared mock_loc ),
        [
          Typed_ast.Variable
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
              Var_name.of_string "x" );
          Typed_ast.Variable
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  Unique mock_loc ),
              Var_name.of_string "y" );
        ] )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [y]
    Reuse

    Fip Expr: UnboxedValue
        Borrowed - []
        Owned - []
        Reuse

        Fip Value: Variable - x
        Borrowed - []
        Owned - [y]
        Reuse

        Fip Value: Variable - y |}]

let%expect_test "FIP rules for expressions : Let" =
  let expr =
    Typed_ast.Let
      ( mock_loc,
        TAttr
          ( mock_loc,
            TETuple
              ( mock_loc,
                [
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc );
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
                  TAttr
                    ( mock_loc,
                      TEPoly (mock_loc, Poly (mock_loc, "'t1")),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u3")) );
                ] ),
            PolyUnique (mock_loc, Poly (mock_loc, "'u4")) ),
        [
          TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
          TAttr
            ( mock_loc,
              TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
              Unique mock_loc );
        ],
        [ Var_name.of_string "x1"; Var_name.of_string "x2" ],
        Typed_ast.UnboxedTuple
          ( mock_loc,
            TAttr
              ( mock_loc,
                TETuple
                  ( mock_loc,
                    [
                      TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc );
                    ] ),
                PolyUnique (mock_loc, Poly (mock_loc, "'u0")) ),
            [
              Typed_ast.Integer
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TEInt mock_loc,
                      PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ),
                  0 );
              Typed_ast.Constructor
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ),
                  Constructor_name.of_string "C",
                  [
                    Typed_ast.Boolean
                      ( mock_loc,
                        TAttr (mock_loc, TEBool mock_loc, Shared mock_loc),
                        true );
                  ] );
            ] ),
        Typed_ast.UnboxedTuple
          ( mock_loc,
            TAttr
              ( mock_loc,
                TETuple
                  ( mock_loc,
                    [
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          PolyUnique (mock_loc, Poly (mock_loc, "'u2")) );
                      TAttr
                        ( mock_loc,
                          TEInt mock_loc,
                          PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
                      TAttr
                        ( mock_loc,
                          TEPoly (mock_loc, Poly (mock_loc, "'t1")),
                          PolyUnique (mock_loc, Poly (mock_loc, "'u3")) );
                    ] ),
                PolyUnique (mock_loc, Poly (mock_loc, "'u4")) ),
            [
              Typed_ast.Variable
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc ),
                  Var_name.of_string "x2" );
              Typed_ast.Variable
                ( mock_loc,
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
                  Var_name.of_string "x1" );
              Typed_ast.Variable
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TEPoly (mock_loc, Poly (mock_loc, "'t1")),
                      Unique mock_loc ),
                  Var_name.of_string "x" );
            ] ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x]
    Reuse
    Key = 1 :: Count = 1 - [_todo]
    Fip Expr: Let vars: (x1, x2) =
        Borrowed - [x]
        Owned - []
        Reuse
        Key = 1 :: Count = 1 - [_todo]
        Fip Expr: UnboxedValue
            Borrowed - [x]
            Owned - []
            Reuse

            Fip Value: Integer - 0
            Borrowed - [x]
            Owned - []
            Reuse
            Key = 1 :: Count = 1 - [_todo]
            Fip Value: Constructor - C
                Borrowed - [x]
                Owned - []
                Reuse

                Fip Value: Boolean - true
    Fip Expr: Let expr:
        Borrowed - []
        Owned - [x, x2]
        Reuse

        Fip Expr: UnboxedValue
            Borrowed - []
            Owned - [x2]
            Reuse

            Fip Value: Variable - x2
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x1
            Borrowed - []
            Owned - [x]
            Reuse

            Fip Value: Variable - x |}]

let%expect_test "FIP rules for expressions : FunApp" =
  let expr =
    Typed_ast.FunApp
      ( mock_loc,
        TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
        Var_name.of_string "y",
        [
          Typed_ast.Variable
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  Unique mock_loc ),
              Var_name.of_string "x" );
        ] )
  in
  let fip_expr =
    Or_error.ok_exn
      (fip_rules_check_expr expr
         (BorrowedSet.singleton (Var_name.of_string "y"))
         functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - [y]
    Owned - [x]
    Reuse

    Fip Expr: FunApp
        FunctionVar: y
        FunctionArg
            Borrowed - [y]
            Owned - [x]
            Reuse

            Fip Value: Variable - x |}]

let%expect_test "FIP rules for expressions : FunCall" =
  let expr =
    Typed_ast.FunCall
      ( mock_loc,
        TAttr
          ( mock_loc,
            TEInt mock_loc,
            PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ),
        Function_name.of_string "apply_fun",
        [
          Typed_ast.Variable
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TEArrow
                    ( mock_loc,
                      TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
                      TAttr
                        ( mock_loc,
                          TEInt mock_loc,
                          PolyUnique (mock_loc, Poly (mock_loc, "'u2")) ) ),
                  Shared mock_loc ),
              Var_name.of_string "add_fun" );
          Typed_ast.Variable
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
              Var_name.of_string "x" );
        ] )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - []
    Reuse

    Fip Expr: FunCall
        Function Name: apply_fun!
        FunctionArg
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - add_fun
        FunctionArg
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x |}]

let%expect_test "FIP rules for expressions : If" =
  let expr =
    Typed_ast.If
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr (mock_loc, TEBool mock_loc, Shared mock_loc),
            Typed_ast.Variable
              ( mock_loc,
                TAttr (mock_loc, TEBool mock_loc, Shared mock_loc),
                Var_name.of_string "x1" ) ),
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Var_name.of_string "x2" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x2]
    Reuse

    Fip Expr: If
        Borrowed - []
        Owned - []
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x1
    Fip Expr: Then
        Borrowed - []
        Owned - [x2]
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x2]
            Reuse

            Fip Value: Variable - x2 |}]

let%expect_test "FIP rules for expressions : IfElse" =
  let expr =
    Typed_ast.IfElse
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr (mock_loc, TEBool mock_loc, Unique mock_loc),
            Typed_ast.Variable
              ( mock_loc,
                TAttr (mock_loc, TEBool mock_loc, Unique mock_loc),
                Var_name.of_string "x1" ) ),
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Var_name.of_string "x2" ) ),
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Var_name.of_string "x2" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x2]
    Reuse

    Fip Expr: If
        Borrowed - []
        Owned - []
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x1
    Fip Expr: Then
        Borrowed - []
        Owned - [x2]
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x2]
            Reuse

            Fip Value: Variable - x2
    Fip Expr: Else
        Borrowed - []
        Owned - [x2]
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x2]
            Reuse

            Fip Value: Variable - x2 |}]

let%expect_test "FIP rules for expressions : Match (borrowed)" =
  let expr =
    Typed_ast.Match
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        Var_name.of_string "y",
        [
          Typed_ast.MPattern
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
              Typed_ast.MConstructor
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Constructor_name.of_string "Atom",
                  [] ),
              UnboxedSingleton
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Typed_ast.Constructor
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                      Constructor_name.of_string "Atom",
                      [] ) ) );
          Typed_ast.MPattern
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
              Typed_ast.MConstructor
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Constructor_name.of_string "C",
                  [
                    Typed_ast.MVariable
                      ( mock_loc,
                        TAttr
                          ( mock_loc,
                            TEBool mock_loc,
                            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                        Var_name.of_string "x1" );
                    Typed_ast.MUnderscore
                      ( mock_loc,
                        TAttr
                          ( mock_loc,
                            TEInt mock_loc,
                            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ) );
                  ] ),
              UnboxedSingleton
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Typed_ast.Constructor
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                      Constructor_name.of_string "Atom",
                      [] ) ) );
        ] )
  in
  let fip_expr =
    Or_error.ok_exn
      (fip_rules_check_expr expr
         (BorrowedSet.singleton (Var_name.of_string "y"))
         functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - [y]
    Owned - []
    Reuse

    Fip Expr: Match
        Match Var: y
        Borrowed - [y]
        Owned - []
        Reuse

        PatternExpr
            Typed MatchedExpr - ( ;  ; ) custom_type @ 'u : Atom
        PatternMatchExpr
            Borrowed - [y]
            Owned - []
            Reuse

            Fip Expr: UnboxedSingleton
                Borrowed - [y]
                Owned - []
                Reuse

                Fip Value: Constructor - Atom
        Borrowed - [y]
        Owned - []
        Reuse

        PatternExpr
            Typed MatchedExpr - ( ;  ; ) custom_type @ 'u : C
                Typed MatchedExpr - Bool @ 'u : Var x1
                Typed MatchedExpr - Int @ 'u : Underscore
        PatternMatchExpr
            Borrowed - [x1, y]
            Owned - []
            Reuse

            Fip Expr: UnboxedSingleton
                Borrowed - [x1, y]
                Owned - []
                Reuse

                Fip Value: Constructor - Atom |}]

let%expect_test "FIP rules for expressions : Match (owned)" =
  let expr =
    Typed_ast.Match
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        Var_name.of_string "x",
        [
          Typed_ast.MPattern
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  Unique mock_loc ),
              Typed_ast.MConstructor
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc ),
                  Constructor_name.of_string "Atom",
                  [] ),
              UnboxedSingleton
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc ),
                  Typed_ast.Constructor
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc ),
                      Constructor_name.of_string "Atom",
                      [] ) ) );
          Typed_ast.MPattern
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  Unique mock_loc ),
              Typed_ast.MConstructor
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc ),
                  Constructor_name.of_string "C",
                  [
                    Typed_ast.MVariable
                      ( mock_loc,
                        TAttr
                          ( mock_loc,
                            TECustom
                              ( mock_loc,
                                [],
                                [],
                                [],
                                Type_name.of_string "custom_type" ),
                            Unique mock_loc ),
                        Var_name.of_string "x1" );
                    Typed_ast.MUnderscore
                      ( mock_loc,
                        TAttr (mock_loc, TEInt mock_loc, Shared mock_loc) );
                    Typed_ast.MConstructor
                      ( mock_loc,
                        TAttr
                          ( mock_loc,
                            TECustom
                              ( mock_loc,
                                [],
                                [],
                                [],
                                Type_name.of_string "custom_type" ),
                            Unique mock_loc ),
                        Constructor_name.of_string "C2",
                        [
                          Typed_ast.MUnderscore
                            ( mock_loc,
                              TAttr (mock_loc, TEInt mock_loc, Shared mock_loc)
                            );
                          Typed_ast.MUnderscore
                            ( mock_loc,
                              TAttr
                                ( mock_loc,
                                  TEInt mock_loc,
                                  PolyUnique (mock_loc, Poly (mock_loc, "'u4"))
                                ) );
                        ] );
                  ] ),
              UnboxedSingleton
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Typed_ast.Constructor
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc ),
                      Constructor_name.of_string "C",
                      [
                        Typed_ast.Variable
                          ( mock_loc,
                            TAttr
                              ( mock_loc,
                                TECustom
                                  ( mock_loc,
                                    [],
                                    [],
                                    [],
                                    Type_name.of_string "custom_type" ),
                                Unique mock_loc ),
                            Var_name.of_string "x1" );
                        Typed_ast.Integer
                          ( mock_loc,
                            TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
                            0 );
                        Typed_ast.Constructor
                          ( mock_loc,
                            TAttr
                              ( mock_loc,
                                TECustom
                                  ( mock_loc,
                                    [],
                                    [],
                                    [],
                                    Type_name.of_string "custom_type" ),
                                PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                            Constructor_name.of_string "C2",
                            [
                              Typed_ast.Integer
                                ( mock_loc,
                                  TAttr
                                    ( mock_loc,
                                      TEInt mock_loc,
                                      PolyUnique
                                        (mock_loc, Poly (mock_loc, "'u2")) ),
                                  1 );
                              Typed_ast.Integer
                                ( mock_loc,
                                  TAttr
                                    ( mock_loc,
                                      TEInt mock_loc,
                                      PolyUnique
                                        (mock_loc, Poly (mock_loc, "'u3")) ),
                                  2 );
                            ] );
                      ] ) ) );
          Typed_ast.MPattern
            ( mock_loc,
              TAttr
                ( mock_loc,
                  TECustom
                    (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                  Unique mock_loc ),
              Typed_ast.MVariable
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                  Var_name.of_string "t" ),
              UnboxedSingleton
                ( mock_loc,
                  TAttr
                    ( mock_loc,
                      TECustom
                        (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                      Unique mock_loc ),
                  Typed_ast.Variable
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc ),
                      Var_name.of_string "t" ) ) );
        ] )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x]
    Reuse

    Fip Expr: DMatch
        Match Var: x
        Borrowed - []
        Owned - []
        Reuse

        PatternExpr
            Typed MatchedExpr - ( ;  ; ) custom_type @ unique : Atom
        PatternMatchExpr
            Borrowed - []
            Owned - []
            Reuse

            Fip Expr: UnboxedSingleton
                Borrowed - []
                Owned - []
                Reuse

                Fip Value: Constructor - Atom
        Borrowed - []
        Owned - []
        Reuse

        PatternExpr
            Typed MatchedExpr - ( ;  ; ) custom_type @ unique : C
                Typed MatchedExpr - ( ;  ; ) custom_type @ unique : Var x1
                Typed MatchedExpr - Int @ shared : Underscore
                Typed MatchedExpr - ( ;  ; ) custom_type @ unique : C2
                    Typed MatchedExpr - Int @ shared : Underscore
                    Typed MatchedExpr - Int @ 'u4 : Underscore
        PatternMatchExpr
            Borrowed - []
            Owned - [x1]
            Reuse
            Key = 2 :: Count = 1 - [_todo]
            Key = 3 :: Count = 1 - [_todo]
            Fip Expr: UnboxedSingleton
                Borrowed - []
                Owned - [x1]
                Reuse
                Key = 2 :: Count = 1 - [_todo]
                Key = 3 :: Count = 1 - [_todo]
                Fip Value: Constructor - C
                    Borrowed - []
                    Owned - [x1]
                    Reuse

                    Fip Value: Variable - x1
                    Borrowed - []
                    Owned - []
                    Reuse

                    Fip Value: Integer - 0
                    Borrowed - []
                    Owned - []
                    Reuse
                    Key = 2 :: Count = 1 - [_todo]
                    Fip Value: Constructor - C2
                        Borrowed - []
                        Owned - []
                        Reuse

                        Fip Value: Integer - 1
                        Borrowed - []
                        Owned - []
                        Reuse

                        Fip Value: Integer - 2
        Borrowed - []
        Owned - []
        Reuse

        PatternExpr
            Typed MatchedExpr - ( ;  ; ) custom_type @ 'u : Var t
        PatternMatchExpr
            Borrowed - []
            Owned - [t]
            Reuse

            Fip Expr: UnboxedSingleton
                Borrowed - []
                Owned - [t]
                Reuse

                Fip Value: Variable - t |}]

let%expect_test "FIP rules for expressions : UnOp" =
  let expr =
    Typed_ast.UnOp
      ( mock_loc,
        TAttr
          ( mock_loc,
            TEBool mock_loc,
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        UnOpNot,
        UnboxedSingleton
          ( mock_loc,
            TAttr (mock_loc, TEBool mock_loc, Unique mock_loc),
            Variable
              ( mock_loc,
                TAttr (mock_loc, TEBool mock_loc, Unique mock_loc),
                Var_name.of_string "x" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - []
    Reuse

    Fip Expr: !
        Borrowed - []
        Owned - []
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x |}]

let%expect_test "FIP rules for expressions : BinOp" =
  let expr =
    Typed_ast.BinaryOp
      ( mock_loc,
        TAttr
          ( mock_loc,
            TEBool mock_loc,
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        BinOpAnd,
        UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TEBool mock_loc,
                PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
            Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TEBool mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                Var_name.of_string "x1" ) ),
        UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TEBool mock_loc,
                PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
            Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TEBool mock_loc,
                    PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                Var_name.of_string "x2" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - []
    Reuse

    Fip Expr: &&
        Borrowed - []
        Owned - []
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x1
        Borrowed - []
        Owned - []
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - []
            Reuse

            Fip Value: Variable - x2 |}]

let%expect_test "FIP rules for expressions : Drop" =
  let expr =
    Typed_ast.Drop
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        Var_name.of_string "x1",
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Var_name.of_string "x2" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fbip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x1, x2]
    Reuse

    Fip Expr: Drop - x1
        Borrowed - []
        Owned - [x2]
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x2]
            Reuse

            Fip Value: Variable - x2 |}]

let%expect_test "FIP rules for expressions : Free" =
  let expr =
    Typed_ast.Free
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        2,
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Variable
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Var_name.of_string "x1" ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fbip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x1]
    Reuse
    Key = 2 :: Count = 1 - [_free]
    Fip Expr: Free - 2
        Borrowed - []
        Owned - [x1]
        Reuse

        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x1]
            Reuse

            Fip Value: Variable - x1 |}]

let%expect_test "FIP rules for expressions : Weak" =
  let expr =
    Typed_ast.Weak
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            Unique mock_loc ),
        2,
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Unique mock_loc ),
            Typed_ast.Constructor
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    Unique mock_loc ),
                Constructor_name.of_string "C",
                [
                  Typed_ast.Variable
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc ),
                      Var_name.of_string "x1" );
                ] ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x1]
    Reuse
    Key = -1 :: Count = 2 - [_new, _new]
    Key = 1 :: Count = 1 - [_todo]
    Fip Expr: Weak - 2
        Borrowed - []
        Owned - [x1]
        Reuse
        Key = 1 :: Count = 1 - [_todo]
        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x1]
            Reuse
            Key = 1 :: Count = 1 - [_todo]
            Fip Value: Constructor - C
                Borrowed - []
                Owned - [x1]
                Reuse

                Fip Value: Variable - x1 |}]

let%expect_test "FIP rules for expressions : Inst" =
  let expr =
    Typed_ast.Inst
      ( mock_loc,
        TAttr
          ( mock_loc,
            TECustom (mock_loc, [], [], [], Type_name.of_string "custom_type"),
            PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
        1,
        Typed_ast.UnboxedSingleton
          ( mock_loc,
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
            Typed_ast.Constructor
              ( mock_loc,
                TAttr
                  ( mock_loc,
                    TECustom
                      (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                    PolyUnique (mock_loc, Poly (mock_loc, "'u")) ),
                Constructor_name.of_string "C",
                [
                  Typed_ast.Variable
                    ( mock_loc,
                      TAttr
                        ( mock_loc,
                          TECustom
                            ( mock_loc,
                              [],
                              [],
                              [],
                              Type_name.of_string "custom_type" ),
                          Unique mock_loc ),
                      Var_name.of_string "x1" );
                ] ) ) )
  in
  let fip_expr =
    Or_error.ok_exn (fip_rules_check_expr expr BorrowedSet.empty functions_env)
  in
  pprint_fip_expr Fmt.stdout ~indent:"" fip_expr;
  [%expect
    {|
    Borrowed - []
    Owned - [x1]
    Reuse
    Key = -1 :: Count = 1 - [_new]
    Fip Expr: Inst - 1
        Borrowed - []
        Owned - [x1]
        Reuse
        Key = 1 :: Count = 1 - [_todo]
        Fip Expr: UnboxedSingleton
            Borrowed - []
            Owned - [x1]
            Reuse
            Key = 1 :: Count = 1 - [_todo]
            Fip Value: Constructor - C
                Borrowed - []
                Owned - [x1]
                Reuse

                Fip Value: Variable - x1 |}]
