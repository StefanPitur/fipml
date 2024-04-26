open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing
open Typing.Type_infer_types
open Typing.Type_infer

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

(* Expect Tests for value constraints generation *)
let%expect_test "Constraints Generation Value: Unit" =
  let value_unit = Unit mock_loc in
  (match
     generate_constraints_value_expr [] [] [] [] value_unit ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Unit
    => Value Ty:
    TyAttr - TyUnit <> TyVarUnique u1
    => Value Constraints:
    => Value Uniqueness Constraints:
    -------------------------

    Value: Unit - TyAttr - TyUnit <> TyVarUnique u1 |}]

let%expect_test "Constraints Generation Value: Int" =
  let value_int = Integer (mock_loc, 0) in
  (match
     generate_constraints_value_expr [] [] [] [] value_int ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Int: 0
    => Value Ty:
    TyAttr - TyInt <> TyVarUnique u2
    => Value Constraints:
    => Value Uniqueness Constraints:
    -------------------------

    Value: Int: 0 - TyAttr - TyInt <> TyVarUnique u2 |}]

let%expect_test "Constraints Generation Value: Atom Constructor" =
  let value_atom =
    Constructor (mock_loc, Constructor_name.of_string "AtomConstructor", [])
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ( [ TEPoly (mock_loc, Poly (mock_loc, "'t1")) ],
          [
            PolyUnique (mock_loc, Poly (mock_loc, "'u1"));
            PolyUnique (mock_loc, Poly (mock_loc, "'u2"));
          ],
          [ TPoly (Poly (mock_loc, "'a1")) ],
          Type_name.of_string "custom_type" );
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "AtomConstructor",
          [] );
    ]
  in
  (match
     generate_constraints_value_expr types_env constructors_env [] [] value_atom
       ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Constructor: AtomConstructor
        ()
    => Value Ty:
    TyAttr - TyVar t1 <> TyVarUnique u3
    => Value Constraints:
    (TyVar t1, TyCustom (TyVar t2 ; TyVarUnique u4, TyVarUnique u5 ; TyAttr - TyVar t3 <> TyVarUnique u6) custom_type)
    => Value Uniqueness Constraints:
    -------------------------

    Value: Constructor: AtomConstructor - TyAttr - TyVar t1 <> TyVarUnique u3
        () |}]

let%expect_test "Constraints Generation Value: Complex Constructor" =
  let value_constructor =
    Constructor
      ( mock_loc,
        Constructor_name.of_string "Constructor",
        [
          Boolean (mock_loc, true);
          Integer (mock_loc, 0);
          Constructor (mock_loc, Constructor_name.of_string "Atom", []);
          Unit mock_loc;
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ( [ TEPoly (mock_loc, Poly (mock_loc, "'t1")) ],
          [
            PolyUnique (mock_loc, Poly (mock_loc, "'u1"));
            PolyUnique (mock_loc, Poly (mock_loc, "'u2"));
          ],
          [ TPoly (Poly (mock_loc, "'a1")) ],
          Type_name.of_string "custom_type" );
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "Atom",
          [] );
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "Constructor",
          [
            TAttr
              ( mock_loc,
                TEPoly (mock_loc, Poly (mock_loc, "'t1")),
                PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
            TAttr
              ( mock_loc,
                TEInt mock_loc,
                PolyUnique (mock_loc, Poly (mock_loc, "'u2")) );
            TAttr
              ( mock_loc,
                TECustom
                  ( mock_loc,
                    [ TEInt mock_loc ],
                    [
                      PolyUnique (mock_loc, Poly (mock_loc, "'u2"));
                      Shared mock_loc;
                    ],
                    [
                      TAttr
                        ( mock_loc,
                          TEBool mock_loc,
                          PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
                    ],
                    Type_name.of_string "custom_type" ),
                Unique mock_loc );
            TPoly (Poly (mock_loc, "'a1"));
          ] );
    ]
  in
  (match
     generate_constraints_value_expr types_env constructors_env [] []
       value_constructor ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
      Actual value:
      Value: Unit
      => Value Ty:
      TyAttr - TyUnit <> TyVarUnique u11
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Constructor: Atom
          ()
      => Value Ty:
      TyAttr - TyVar t7 <> TyVarUnique u12
      => Value Constraints:
      (TyVar t7, TyCustom (TyVar t8 ; TyVarUnique u13, TyVarUnique u14 ; TyAttr - TyVar t9 <> TyVarUnique u15) custom_type)
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Int: 0
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u16
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Bool: true
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u17
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Constructor: Constructor
          ConstructorArg
              Value: Bool: true
          ConstructorArg
              Value: Int: 0
          ConstructorArg
              Value: Constructor: Atom
                  ()
          ConstructorArg
              Value: Unit
      => Value Ty:
      TyAttr - TyVar t4 <> TyVarUnique u7
      => Value Constraints:
      (TyVar t4, TyCustom (TyVar t5 ; TyVarUnique u8, TyVarUnique u9 ; TyAttr - TyVar t6 <> TyVarUnique u10) custom_type)
      (TyBool, TyVar t5)
      (TyInt, TyInt)
      (TyVar t7, TyCustom (TyInt ; TyVarUnique u9, TyShared ; TyAttr - TyBool <> TyVarUnique u8) custom_type)
      (TyVar t7, TyCustom (TyVar t8 ; TyVarUnique u13, TyVarUnique u14 ; TyAttr - TyVar t9 <> TyVarUnique u15) custom_type)
      (TyUnit, TyVar t6)
      => Value Uniqueness Constraints:
      (TyVarUnique u17, TyVarUnique u8)
      (TyVarUnique u16, TyVarUnique u9)
      (TyVarUnique u12, TyUnique)
      (TyVarUnique u11, TyVarUnique u10)
      -------------------------

      Value: Constructor: Constructor - TyAttr - TyVar t4 <> TyVarUnique u7
              ConstructorArg
              Value: Bool: true - TyAttr - TyBool <> TyVarUnique u17
              ConstructorArg
              Value: Int: 0 - TyAttr - TyInt <> TyVarUnique u16
              ConstructorArg
              Value: Constructor: Atom - TyAttr - TyVar t7 <> TyVarUnique u12
                  ()
              ConstructorArg
              Value: Unit - TyAttr - TyUnit <> TyVarUnique u11 |}]

let%expect_test "Constraints Generation Value: Variable" =
  let value_var = Variable (mock_loc, Var_name.of_string "x") in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyInt, fresh_unique ()));
    ]
  in
  (match
     generate_constraints_value_expr [] [] [] typing_context value_var
       ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u18
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Value: Var: x - TyAttr - TyInt <> TyVarUnique u18 |}]

(* Expect Test for expr constraints generation *)
let%expect_test "Constraints Generation Expr: UnboxedSingleton" =
  let expr_singleton =
    UnboxedSingleton
      ( mock_loc,
        Constructor
          ( mock_loc,
            Constructor_name.of_string "C",
            [
              Boolean (mock_loc, true);
              Variable (mock_loc, Var_name.of_string "x");
            ] ) )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ([], [], [], Type_name.of_string "custom_type");
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [
            TAttr (mock_loc, TEBool mock_loc, Shared mock_loc);
            TAttr (mock_loc, TEInt mock_loc, Unique mock_loc);
          ] );
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyInt, fresh_unique ()));
    ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_singleton ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u19
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Bool: true
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u21
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Constructor: C
          ConstructorArg
              Value: Bool: true
          ConstructorArg
              Value: Var: x
      => Value Ty:
      TyAttr - TyVar t10 <> TyVarUnique u20
      => Value Constraints:
      (TyVar t10, TyCustom ( ;  ; ) custom_type)
      (TyBool, TyBool)
      (TyInt, TyInt)
      => Value Uniqueness Constraints:
      (TyVarUnique u21, TyShared)
      (TyVarUnique u19, TyUnique)
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Constructor: C
              ConstructorArg
                  Value: Bool: true
              ConstructorArg
                  Value: Var: x

      => Typing Context:
      x : TyAttr - TyInt <> TyVarUnique u19
      => Expr TyAttr:
      TyAttr - TyVar t10 <> TyVarUnique u20
      => Expr Constraints:
      (TyVar t10, TyCustom ( ;  ; ) custom_type)
      (TyBool, TyBool)
      (TyInt, TyInt)
      => Expr Unique Constraints;
      (TyVarUnique u21, TyShared)
      (TyVarUnique u19, TyUnique)
      -------------------------

      Pretyped Expr: UnboxedSingleton - TyAttr - TyVar t10 <> TyVarUnique u20
          Value: Constructor: C - TyAttr - TyVar t10 <> TyVarUnique u20
                  ConstructorArg
                  Value: Bool: true - TyAttr - TyBool <> TyVarUnique u21
                  ConstructorArg
                  Value: Var: x - TyAttr - TyInt <> TyVarUnique u19 |}]

let%expect_test "Constraints Generation Expr: UnboxedTuple" =
  let expr_tuple =
    UnboxedTuple
      ( mock_loc,
        [
          Unit mock_loc;
          Boolean (mock_loc, true);
          Constructor
            ( mock_loc,
              Constructor_name.of_string "C",
              [ Variable (mock_loc, Var_name.of_string "x") ] );
          Variable (mock_loc, Var_name.of_string "y");
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ([], [], [], Type_name.of_string "custom_type");
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [ TAttr (mock_loc, TEBool mock_loc, Unique mock_loc) ] );
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyBool, fresh_unique ()));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", (fresh (), fresh_unique ()));
    ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_tuple ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyVar t11 <> TyVarUnique u22
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u23
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Constructor: C
          ConstructorArg
              Value: Var: x
      => Value Ty:
      TyAttr - TyVar t12 <> TyVarUnique u25
      => Value Constraints:
      (TyVar t12, TyCustom ( ;  ; ) custom_type)
      (TyBool, TyBool)
      => Value Uniqueness Constraints:
      (TyVarUnique u23, TyUnique)
      -------------------------

      Actual value:
      Value: Bool: true
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u26
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Unit
      => Value Ty:
      TyAttr - TyUnit <> TyVarUnique u27
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Unit
          Value: Bool: true
          Value: Constructor: C
              ConstructorArg
                  Value: Var: x
          Value: Var: y

      => Typing Context:
      x : TyAttr - TyBool <> TyVarUnique u23
      y : TyAttr - TyVar t11 <> TyVarUnique u22
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyUnit <> TyVarUnique u27, TyAttr - TyBool <> TyVarUnique u26, TyAttr - TyVar t12 <> TyVarUnique u25, TyAttr - TyVar t11 <> TyVarUnique u22) <> TyVarUnique u24
      => Expr Constraints:
      (TyVar t12, TyCustom ( ;  ; ) custom_type)
      (TyBool, TyBool)
      => Expr Unique Constraints;
      (TyVarUnique u23, TyUnique)
      -------------------------

      Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyUnit <> TyVarUnique u27, TyAttr - TyBool <> TyVarUnique u26, TyAttr - TyVar t12 <> TyVarUnique u25, TyAttr - TyVar t11 <> TyVarUnique u22) <> TyVarUnique u24
          Value: Unit - TyAttr - TyUnit <> TyVarUnique u27
          Value: Bool: true - TyAttr - TyBool <> TyVarUnique u26
          Value: Constructor: C - TyAttr - TyVar t12 <> TyVarUnique u25
                  ConstructorArg
                  Value: Var: x - TyAttr - TyBool <> TyVarUnique u23
          Value: Var: y - TyAttr - TyVar t11 <> TyVarUnique u22 |}]

let%expect_test "Constraints Generation Expr: Let" =
  let expr_let =
    Let
      ( mock_loc,
        [ Var_name.of_string "x"; Var_name.of_string "y" ],
        UnboxedTuple
          ( mock_loc,
            [
              Integer (mock_loc, 0);
              Constructor (mock_loc, Constructor_name.of_string "Atom", []);
            ] ),
        UnboxedTuple
          ( mock_loc,
            [
              Variable (mock_loc, Var_name.of_string "x");
              Variable (mock_loc, Var_name.of_string "x");
              Variable (mock_loc, Var_name.of_string "y");
            ] ) )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ( [ TEPoly (mock_loc, Poly (mock_loc, "'t1")) ],
          [ PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ],
          [ TPoly (Poly (mock_loc, "'a1")) ],
          Type_name.of_string "custom_type" );
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "Atom",
          [] );
    ]
  in
  (match
     generate_constraints types_env constructors_env [] [] expr_let
       ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Constructor: Atom
          ()
      => Value Ty:
      TyAttr - TyVar t13 <> TyVarUnique u29
      => Value Constraints:
      (TyVar t13, TyCustom (TyVar t14 ; TyVarUnique u30 ; TyAttr - TyVar t15 <> TyVarUnique u31) custom_type)
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Int: 0
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u32
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Int: 0
          Value: Constructor: Atom
              ()

      => Typing Context:
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyVar t13 <> TyVarUnique u29) <> TyVarUnique u28
      => Expr Constraints:
      (TyVar t13, TyCustom (TyVar t14 ; TyVarUnique u30 ; TyAttr - TyVar t15 <> TyVarUnique u31) custom_type)
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u32
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u32
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Var: x
          Value: Var: x
          Value: Var: y

      => Typing Context:
      x : TyAttr - TyPoly - for all (). TyInt <> TyVarUnique u32
      y : TyAttr - TyPoly - for all (t14, t15). TyCustom (TyVar t14 ; TyVarUnique u30 ; TyAttr - TyVar t15 <> TyVarUnique u31) custom_type <> TyVarUnique u29
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29) <> TyVarUnique u34
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Let vars: (x, y) =
          Expr: UnboxedTuple
              Value: Int: 0
              Value: Constructor: Atom
                  ()
          Expr: UnboxedTuple
              Value: Var: x
              Value: Var: x
              Value: Var: y

      => Typing Context:
      x : TyAttr - TyPoly - for all (). TyInt <> TyVarUnique u32
      y : TyAttr - TyPoly - for all (t14, t15). TyCustom (TyVar t14 ; TyVarUnique u30 ; TyAttr - TyVar t15 <> TyVarUnique u31) custom_type <> TyVarUnique u29
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29) <> TyVarUnique u34
      => Expr Constraints:
      => Expr Unique Constraints;
      (TyShared, TyVarUnique u32)
      (TyVarUnique u33, TyVarUnique u29)
      -------------------------

      Pretyped Expr: Let vars: (x, y) : (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyCustom (TyVar t14 ; TyVarUnique u30 ; TyAttr - TyVar t15 <> TyVarUnique u31) custom_type <> TyVarUnique u29) =
          Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyVar t13 <> TyVarUnique u29) <> TyVarUnique u28
              Value: Int: 0 - TyAttr - TyInt <> TyVarUnique u32
              Value: Constructor: Atom - TyAttr - TyVar t13 <> TyVarUnique u29
                  ()
      Pretyped Expr: Let expr - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29) <> TyVarUnique u34
          Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyInt <> TyVarUnique u32, TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29) <> TyVarUnique u34
              Value: Var: x - TyAttr - TyInt <> TyVarUnique u32
              Value: Var: x - TyAttr - TyInt <> TyVarUnique u32
              Value: Var: y - TyAttr - TyCustom (TyVar t16 ; TyVarUnique u30 ; TyAttr - TyVar t17 <> TyVarUnique u31) custom_type <> TyVarUnique u29 |}]

let%expect_test "Constraints Generation Expr: FunApp" =
  let expr_funapp =
    FunApp
      ( mock_loc,
        Var_name.of_string "f",
        [
          Variable (mock_loc, Var_name.of_string "bool_to_bool_fun");
          Variable (mock_loc, Var_name.of_string "x");
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ([], [], [], Type_name.of_string "custom_type");
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "x",
          (TyCustom ([], [], [], Type_name.of_string "custom_type"), TyShared)
        );
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "bool_to_bool_fun",
          ( TyArrow ((TyBool, TyUnique), (TyBool, fresh_unique ())),
            fresh_unique () ) );
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "f",
          ( TyArrow
              ( ( TyArrow ((TyBool, TyUnique), (TyBool, fresh_unique ())),
                  fresh_unique () ),
                ( TyArrow
                    ( ( TyCustom ([], [], [], Type_name.of_string "custom_type"),
                        TyShared ),
                      ( TyTuple
                          [
                            (TyInt, fresh_unique ());
                            (TyBool, fresh_unique ());
                            ( TyCustom
                                ([], [], [], Type_name.of_string "custom_type"),
                              fresh_unique () );
                          ],
                        fresh_unique () ) ),
                  fresh_unique () ) ),
            fresh_unique () ) );
    ]
  in
  (match
     generate_constraints types_env [] [] typing_context expr_funapp
       ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyCustom ( ;  ; ) custom_type <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: bool_to_bool_fun
      => Value Ty:
      TyAttr - TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u44) <> TyVarUnique u43
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: FunApp
          FunctionVar: f
          FunApp Args:
              Value: Var: bool_to_bool_fun
              Value: Var: x

      => Typing Context:
      x : TyAttr - TyCustom ( ;  ; ) custom_type <> TyShared
      bool_to_bool_fun : TyAttr - TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u44) <> TyVarUnique u43
      f : TyAttr - TyArrow (TyAttr - TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u42) <> TyVarUnique u41 -> TyAttr - TyArrow (TyAttr - TyCustom ( ;  ; ) custom_type <> TyShared -> TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u40, TyAttr - TyBool <> TyVarUnique u39, TyAttr - TyCustom ( ;  ; ) custom_type <> TyVarUnique u38) <> TyVarUnique u37) <> TyVarUnique u36) <> TyVarUnique u35
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u40, TyAttr - TyBool <> TyVarUnique u39, TyAttr - TyCustom ( ;  ; ) custom_type <> TyVarUnique u38) <> TyVarUnique u37
      => Expr Constraints:
      (TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u44), TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u42))
      (TyCustom ( ;  ; ) custom_type, TyCustom ( ;  ; ) custom_type)
      => Expr Unique Constraints;
      (TyVarUnique u43, TyVarUnique u41)
      (TyShared, TyShared)
      -------------------------

      Pretyped Expr: FunApp - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u40, TyAttr - TyBool <> TyVarUnique u39, TyAttr - TyCustom ( ;  ; ) custom_type <> TyVarUnique u38) <> TyVarUnique u37
          FunctionVar: f
          FunctionArg
              Value: Var: bool_to_bool_fun - TyAttr - TyArrow (TyAttr - TyBool <> TyUnique -> TyAttr - TyBool <> TyVarUnique u44) <> TyVarUnique u43
          FunctionArg
              Value: Var: x - TyAttr - TyCustom ( ;  ; ) custom_type <> TyShared |}]

let%expect_test "Constraints Generation Expr: FunCall" =
  let expr_funcall =
    FunCall
      ( mock_loc,
        Function_name.of_string "mock_fun",
        [
          Integer (mock_loc, 0);
          Constructor
            (mock_loc, Constructor_name.of_string "C", [ Unit mock_loc ]);
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ([], [], [], Type_name.of_string "custom_type");
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [ TAttr (mock_loc, TEUnit mock_loc, Unique mock_loc) ] );
    ]
  in
  let functions_env =
    [
      Functions_env.FunctionEnvEntry
        ( 1,
          None,
          Function_name.of_string "mock_fun",
          [
            TAttr
              ( mock_loc,
                TEInt mock_loc,
                PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
            TAttr
              ( mock_loc,
                TECustom
                  (mock_loc, [], [], [], Type_name.of_string "custom_type"),
                Shared mock_loc );
          ],
          [ None; None ],
          TAttr
            ( mock_loc,
              TETuple
                ( mock_loc,
                  [
                    TAttr
                      ( mock_loc,
                        TEInt mock_loc,
                        PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
                    TAttr
                      ( mock_loc,
                        TECustom
                          ( mock_loc,
                            [],
                            [],
                            [],
                            Type_name.of_string "custom_type" ),
                        PolyUnique (mock_loc, Poly (mock_loc, "'u2")) );
                  ] ),
              PolyUnique (mock_loc, Poly (mock_loc, "'u3")) ) );
    ]
  in
  (match
     generate_constraints types_env constructors_env functions_env []
       expr_funcall ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Unit
      => Value Ty:
      TyAttr - TyUnit <> TyVarUnique u49
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Constructor: C
          ConstructorArg
              Value: Unit
      => Value Ty:
      TyAttr - TyVar t18 <> TyVarUnique u48
      => Value Constraints:
      (TyVar t18, TyCustom ( ;  ; ) custom_type)
      (TyUnit, TyUnit)
      => Value Uniqueness Constraints:
      (TyVarUnique u49, TyUnique)
      -------------------------

      Actual value:
      Value: Int: 0
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u50
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: FunCall
          Function Name: mock_fun
          FunCall Args:
              Value: Int: 0
              Value: Constructor: C
                  ConstructorArg
                      Value: Unit

      => Typing Context:
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u45, TyAttr - TyCustom ( ;  ; ) custom_type <> TyVarUnique u46) <> TyVarUnique u47
      => Expr Constraints:
      (TyInt, TyInt)
      (TyCustom ( ;  ; ) custom_type, TyVar t18)
      (TyVar t18, TyCustom ( ;  ; ) custom_type)
      (TyUnit, TyUnit)
      => Expr Unique Constraints;
      (TyVarUnique u45, TyVarUnique u50)
      (TyShared, TyVarUnique u48)
      (TyVarUnique u49, TyUnique)
      -------------------------

      Pretyped Expr: FunCall - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u45, TyAttr - TyCustom ( ;  ; ) custom_type <> TyVarUnique u46) <> TyVarUnique u47
          Function Name: mock_fun
          FunctionArg
              Value: Int: 0 - TyAttr - TyInt <> TyVarUnique u50
          FunctionArg
              Value: Constructor: C - TyAttr - TyVar t18 <> TyVarUnique u48
                      ConstructorArg
                      Value: Unit - TyAttr - TyUnit <> TyVarUnique u49 |}]

let%expect_test "Constraints Generation Expr: If" =
  let expr_if =
    If
      ( mock_loc,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
        UnboxedTuple
          ( mock_loc,
            [
              Variable (mock_loc, Var_name.of_string "x");
              Variable (mock_loc, Var_name.of_string "y");
            ] ) )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyBool, TyShared));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", (TyInt, fresh_unique ()));
    ]
  in
  (match generate_constraints [] [] [] typing_context expr_if ~verbose:true with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyBool <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u51
      => Expr TyAttr:
      TyAttr - TyBool <> TyShared
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u51
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyBool <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Var: x
          Value: Var: y

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u51
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u51) <> TyVarUnique u53
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: If
          Expr: UnboxedSingleton
              Value: Var: x
          Then
          Expr: UnboxedTuple
              Value: Var: x
              Value: Var: y

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u51
      => Expr TyAttr:
      TyAttr - TyVar t19 <> TyVarUnique u52
      => Expr Constraints:
      (TyBool, TyBool)
      (TyVar t19, TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u51))
      => Expr Unique Constraints;
      (TyVarUnique u52, TyVarUnique u53)
      -------------------------

      Pretyped Expr: If - TyAttr - TyVar t19 <> TyVarUnique u52
          Pretyped Expr: UnboxedSingleton - TyAttr - TyBool <> TyShared
              Value: Var: x - TyAttr - TyBool <> TyShared
      Then
          Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u51) <> TyVarUnique u53
              Value: Var: x - TyAttr - TyBool <> TyShared
              Value: Var: y - TyAttr - TyInt <> TyVarUnique u51 |}]

let%expect_test "Constraints Generation Expr: IfElse" =
  let expr_if_else =
    IfElse
      ( mock_loc,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x")),
        UnboxedTuple
          ( mock_loc,
            [
              Variable (mock_loc, Var_name.of_string "x");
              Variable (mock_loc, Var_name.of_string "y");
            ] ),
        UnboxedTuple
          (mock_loc, [ Boolean (mock_loc, false); Integer (mock_loc, 1) ]) )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyBool, TyShared));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", (TyInt, fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_if_else ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyBool <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u54
      => Expr TyAttr:
      TyAttr - TyBool <> TyShared
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u54
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyBool <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Var: x
          Value: Var: y

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u54
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u54) <> TyVarUnique u56
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Int: 1
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u58
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Bool: false
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u59
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Bool: false
          Value: Int: 1

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u54
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyBool <> TyVarUnique u59, TyAttr - TyInt <> TyVarUnique u58) <> TyVarUnique u57
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: IfElse
          Expr: UnboxedSingleton
              Value: Var: x
          Then
          Expr: UnboxedTuple
              Value: Var: x
              Value: Var: y
          Else
          Expr: UnboxedTuple
              Value: Bool: false
              Value: Int: 1

      => Typing Context:
      x : TyAttr - TyBool <> TyShared
      y : TyAttr - TyInt <> TyVarUnique u54
      => Expr TyAttr:
      TyAttr - TyVar t20 <> TyVarUnique u55
      => Expr Constraints:
      (TyBool, TyBool)
      (TyVar t20, TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u54))
      (TyVar t20, TyTuple (TyAttr - TyBool <> TyVarUnique u59, TyAttr - TyInt <> TyVarUnique u58))
      => Expr Unique Constraints;
      (TyVarUnique u55, TyVarUnique u56)
      (TyVarUnique u55, TyVarUnique u57)
      -------------------------

      Pretyped Expr: IfElse - TyAttr - TyVar t20 <> TyVarUnique u55
          Pretyped Expr: UnboxedSingleton - TyAttr - TyBool <> TyShared
              Value: Var: x - TyAttr - TyBool <> TyShared
      Then
          Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyBool <> TyShared, TyAttr - TyInt <> TyVarUnique u54) <> TyVarUnique u56
              Value: Var: x - TyAttr - TyBool <> TyShared
              Value: Var: y - TyAttr - TyInt <> TyVarUnique u54
      Else
          Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyBool <> TyVarUnique u59, TyAttr - TyInt <> TyVarUnique u58) <> TyVarUnique u57
              Value: Bool: false - TyAttr - TyBool <> TyVarUnique u59
              Value: Int: 1 - TyAttr - TyInt <> TyVarUnique u58 |}]

let%expect_test "Constraints Generation Expr: Match" =
  let expr_match =
    Match
      ( mock_loc,
        Var_name.of_string "x",
        [
          MPattern
            ( mock_loc,
              MUnderscore mock_loc,
              UnboxedTuple
                (mock_loc, [ Integer (mock_loc, 0); Boolean (mock_loc, true) ])
            );
          MPattern
            ( mock_loc,
              MConstructor
                ( mock_loc,
                  Constructor_name.of_string "C1",
                  [ MVariable (mock_loc, Var_name.of_string "y") ] ),
              UnboxedTuple
                ( mock_loc,
                  [
                    Variable (mock_loc, Var_name.of_string "y");
                    Boolean (mock_loc, true);
                  ] ) );
          MPattern
            ( mock_loc,
              MConstructor
                ( mock_loc,
                  Constructor_name.of_string "C2",
                  [
                    MVariable (mock_loc, Var_name.of_string "y");
                    MConstructor
                      ( mock_loc,
                        Constructor_name.of_string "C1",
                        [ MUnderscore mock_loc ] );
                  ] ),
              UnboxedTuple
                ( mock_loc,
                  [
                    Integer (mock_loc, 1);
                    Variable (mock_loc, Var_name.of_string "y");
                  ] ) );
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ( [],
          [ PolyUnique (mock_loc, Poly (mock_loc, "'u1")) ],
          [],
          Type_name.of_string "custom_type" );
    ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C1",
          [ TAttr (mock_loc, TEInt mock_loc, Unique mock_loc) ] );
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C2",
          [
            TAttr
              ( mock_loc,
                TEBool mock_loc,
                PolyUnique (mock_loc, Poly (mock_loc, "'u1")) );
            TAttr
              ( mock_loc,
                TECustom
                  ( mock_loc,
                    [],
                    [ Unique mock_loc ],
                    [],
                    Type_name.of_string "custom_type" ),
                Shared mock_loc );
          ] );
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "x",
          ( TyCustom
              ([], [ fresh_unique () ], [], Type_name.of_string "custom_type"),
            fresh_unique () ) );
    ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_match ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyVar t23 <> TyVarUnique u68
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Int: 1
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u70
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Int: 1
          Value: Var: y

      => Typing Context:
      y : TyAttr - TyVar t23 <> TyVarUnique u68
      x : TyAttr - TyCustom ( ; TyVarUnique u61 ; ) custom_type <> TyVarUnique u60
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u70, TyAttr - TyVar t23 <> TyVarUnique u68) <> TyVarUnique u69
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Bool: true
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u75
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyVar t24 <> TyVarUnique u73
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Var: y
          Value: Bool: true

      => Typing Context:
      y : TyAttr - TyVar t24 <> TyVarUnique u73
      x : TyAttr - TyCustom ( ; TyVarUnique u61 ; ) custom_type <> TyVarUnique u60
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyVar t24 <> TyVarUnique u73, TyAttr - TyBool <> TyVarUnique u75) <> TyVarUnique u74
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Bool: true
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u78
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual value:
      Value: Int: 0
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u79
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedTuple
          Value: Int: 0
          Value: Bool: true

      => Typing Context:
      x : TyAttr - TyCustom ( ; TyVarUnique u61 ; ) custom_type <> TyVarUnique u60
      => Expr TyAttr:
      TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u79, TyAttr - TyBool <> TyVarUnique u78) <> TyVarUnique u77
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Match
          Match Var: x
          Pattern
              MatchedExpr: Underscore
              PatternExpr
              Expr: UnboxedTuple
                  Value: Int: 0
                  Value: Bool: true
          Pattern
              MatchedExpr: Constructor - C1
                  MatchedExpr: Var - y
              PatternExpr
              Expr: UnboxedTuple
                  Value: Var: y
                  Value: Bool: true
          Pattern
              MatchedExpr: Constructor - C2
                  MatchedExpr: Var - y
                  MatchedExpr: Constructor - C1
                      MatchedExpr: Underscore
              PatternExpr
              Expr: UnboxedTuple
                  Value: Int: 1
                  Value: Var: y

      => Typing Context:
      x : TyAttr - TyCustom ( ; TyVarUnique u61 ; ) custom_type <> TyVarUnique u60
      => Expr TyAttr:
      TyAttr - TyVar t21 <> TyVarUnique u62
      => Expr Constraints:
      (TyCustom ( ; TyVarUnique u61 ; ) custom_type, TyVar t25)
      (TyVar t21, TyTuple (TyAttr - TyInt <> TyVarUnique u79, TyAttr - TyBool <> TyVarUnique u78))
      (TyCustom ( ; TyVarUnique u61 ; ) custom_type, TyCustom ( ; TyVarUnique u72 ; ) custom_type)
      (TyVar t21, TyTuple (TyAttr - TyVar t24 <> TyVarUnique u73, TyAttr - TyBool <> TyVarUnique u75))
      (TyVar t24, TyInt)
      (TyCustom ( ; TyVarUnique u61 ; ) custom_type, TyCustom ( ; TyVarUnique u64 ; ) custom_type)
      (TyVar t21, TyTuple (TyAttr - TyInt <> TyVarUnique u70, TyAttr - TyVar t23 <> TyVarUnique u68))
      (TyVar t23, TyBool)
      (TyCustom ( ; TyVarUnique u66 ; ) custom_type, TyCustom ( ; TyUnique ; ) custom_type)
      (TyVar t22, TyInt)
      => Expr Unique Constraints;
      (TyVarUnique u60, TyVarUnique u76)
      (TyVarUnique u62, TyVarUnique u77)
      (TyVarUnique u60, TyVarUnique u71)
      (TyVarUnique u62, TyVarUnique u74)
      (TyVarUnique u73, TyUnique)
      (TyVarUnique u60, TyVarUnique u63)
      (TyVarUnique u62, TyVarUnique u69)
      (TyVarUnique u68, TyVarUnique u64)
      (TyVarUnique u65, TyShared)
      (TyVarUnique u67, TyUnique)
      -------------------------

      Pretyped Expr: Match - TyAttr - TyVar t21 <> TyVarUnique u62
          Match Var: x - TyAttr - TyCustom ( ; TyVarUnique u61 ; ) custom_type <> TyVarUnique u60
          PatternExpr - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u79, TyAttr - TyBool <> TyVarUnique u78) <> TyVarUnique u77
              Pretyped MatchedExpr - TyAttr - TyVar t25 <> TyVarUnique u76: Underscore
          PatternMatchExpr
              Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u79, TyAttr - TyBool <> TyVarUnique u78) <> TyVarUnique u77
                  Value: Int: 0 - TyAttr - TyInt <> TyVarUnique u79
                  Value: Bool: true - TyAttr - TyBool <> TyVarUnique u78
          PatternExpr - TyAttr - TyTuple (TyAttr - TyVar t24 <> TyVarUnique u73, TyAttr - TyBool <> TyVarUnique u75) <> TyVarUnique u74
              Pretyped MatchedExpr - TyAttr - TyCustom ( ; TyVarUnique u72 ; ) custom_type <> TyVarUnique u71: C1
                  Pretyped MatchedExpr - TyAttr - TyVar t24 <> TyVarUnique u73: Var y
          PatternMatchExpr
              Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyVar t24 <> TyVarUnique u73, TyAttr - TyBool <> TyVarUnique u75) <> TyVarUnique u74
                  Value: Var: y - TyAttr - TyVar t24 <> TyVarUnique u73
                  Value: Bool: true - TyAttr - TyBool <> TyVarUnique u75
          PatternExpr - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u70, TyAttr - TyVar t23 <> TyVarUnique u68) <> TyVarUnique u69
              Pretyped MatchedExpr - TyAttr - TyCustom ( ; TyVarUnique u64 ; ) custom_type <> TyVarUnique u63: C2
                  Pretyped MatchedExpr - TyAttr - TyVar t23 <> TyVarUnique u68: Var y
                  Pretyped MatchedExpr - TyAttr - TyCustom ( ; TyVarUnique u66 ; ) custom_type <> TyVarUnique u65: C1
                      Pretyped MatchedExpr - TyAttr - TyVar t22 <> TyVarUnique u67: Underscore
          PatternMatchExpr
              Pretyped Expr: UnboxedTuple - TyAttr - TyTuple (TyAttr - TyInt <> TyVarUnique u70, TyAttr - TyVar t23 <> TyVarUnique u68) <> TyVarUnique u69
                  Value: Int: 1 - TyAttr - TyInt <> TyVarUnique u70
                  Value: Var: y - TyAttr - TyVar t23 <> TyVarUnique u68 |}]

let%expect_test "Constraints Generation Expr: UnOp" =
  let expr_neg =
    UnOp
      ( mock_loc,
        UnOpNeg,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let expr_not =
    UnOp
      ( mock_loc,
        UnOpNot,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "y"))
      )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyInt, TyUnique));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", (TyBool, fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_neg ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  Fmt.pf Fmt.stdout "\n";
  (match
     generate_constraints [] [] [] typing_context expr_not ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyInt <> TyUnique
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyInt <> TyUnique
      y : TyAttr - TyBool <> TyVarUnique u80
      => Expr TyAttr:
      TyAttr - TyInt <> TyUnique
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Unary Op: -
          Expr: UnboxedSingleton
              Value: Var: x

      => Typing Context:
      x : TyAttr - TyInt <> TyUnique
      y : TyAttr - TyBool <> TyVarUnique u80
      => Expr TyAttr:
      TyAttr - TyInt <> TyVarUnique u81
      => Expr Constraints:
      (TyInt, TyInt)
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: - - TyAttr - TyInt <> TyVarUnique u81
          Pretyped Expr: UnboxedSingleton - TyAttr - TyInt <> TyUnique
              Value: Var: x - TyAttr - TyInt <> TyUnique

      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyBool <> TyVarUnique u80
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: y

      => Typing Context:
      x : TyAttr - TyInt <> TyUnique
      y : TyAttr - TyBool <> TyVarUnique u80
      => Expr TyAttr:
      TyAttr - TyBool <> TyVarUnique u80
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Unary Op: !
          Expr: UnboxedSingleton
              Value: Var: y

      => Typing Context:
      x : TyAttr - TyInt <> TyUnique
      y : TyAttr - TyBool <> TyVarUnique u80
      => Expr TyAttr:
      TyAttr - TyBool <> TyVarUnique u82
      => Expr Constraints:
      (TyBool, TyBool)
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: ! - TyAttr - TyBool <> TyVarUnique u82
          Pretyped Expr: UnboxedSingleton - TyAttr - TyBool <> TyVarUnique u80
              Value: Var: y - TyAttr - TyBool <> TyVarUnique u80 |}]

let%expect_test "Constraints Generation Expr: BinOp" =
  let typing_context_ints : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x1", (TyInt, TyShared));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x2", (TyInt, fresh_unique ()));
    ]
  in
  let bin_op_plus_expr =
    BinaryOp
      ( mock_loc,
        BinOpPlus,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x1")),
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x2"))
      )
  in
  (match
     generate_constraints [] [] [] typing_context_ints bin_op_plus_expr
       ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x1
      => Value Ty:
      TyAttr - TyInt <> TyShared
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x1

      => Typing Context:
      x1 : TyAttr - TyInt <> TyShared
      x2 : TyAttr - TyInt <> TyVarUnique u83
      => Expr TyAttr:
      TyAttr - TyInt <> TyShared
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual value:
      Value: Var: x2
      => Value Ty:
      TyAttr - TyInt <> TyVarUnique u83
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x2

      => Typing Context:
      x1 : TyAttr - TyInt <> TyShared
      x2 : TyAttr - TyInt <> TyVarUnique u83
      => Expr TyAttr:
      TyAttr - TyInt <> TyVarUnique u83
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Binary Op: +
          LeftExpr
          Expr: UnboxedSingleton
              Value: Var: x1
          RightExpr
          Expr: UnboxedSingleton
              Value: Var: x2

      => Typing Context:
      x1 : TyAttr - TyInt <> TyShared
      x2 : TyAttr - TyInt <> TyVarUnique u83
      => Expr TyAttr:
      TyAttr - TyInt <> TyVarUnique u84
      => Expr Constraints:
      (TyInt, TyInt)
      (TyInt, TyInt)
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: + - TyAttr - TyInt <> TyVarUnique u84
          Pretyped Expr: UnboxedSingleton - TyAttr - TyInt <> TyShared
              Value: Var: x1 - TyAttr - TyInt <> TyShared
          Pretyped Expr: UnboxedSingleton - TyAttr - TyInt <> TyVarUnique u83
              Value: Var: x2 - TyAttr - TyInt <> TyVarUnique u83 |}]

let%expect_test "Constraints Generation Expr: Drop" =
  let expr_drop =
    Drop
      ( mock_loc,
        Var_name.of_string "x",
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "y"))
      )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (TyInt, fresh_unique ()));
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", (fresh (), fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_drop ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: y
      => Value Ty:
      TyAttr - TyVar t26 <> TyVarUnique u85
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: y

      => Typing Context:
      y : TyAttr - TyVar t26 <> TyVarUnique u85
      => Expr TyAttr:
      TyAttr - TyVar t26 <> TyVarUnique u85
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Drop x - Expr:
          Expr: UnboxedSingleton
              Value: Var: y

      => Typing Context:
      x : TyAttr - TyInt <> TyVarUnique u86
      y : TyAttr - TyVar t26 <> TyVarUnique u85
      => Expr TyAttr:
      TyAttr - TyVar t26 <> TyVarUnique u85
      => Expr Constraints:
      => Expr Unique Constraints;
      (TyVarUnique u86, TyUnique)
      -------------------------

      Pretyped Expr: Drop (x : TyAttr - TyInt <> TyVarUnique u86) - TyAttr - TyVar t26 <> TyVarUnique u85 Expr:
          Pretyped Expr: UnboxedSingleton - TyAttr - TyVar t26 <> TyVarUnique u85
              Value: Var: y - TyAttr - TyVar t26 <> TyVarUnique u85 |}]

let%expect_test "Constraints Generation Expr: Free" =
  let expr_free =
    Free
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (fresh (), fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_free ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyVar t27 <> TyVarUnique u87
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t27 <> TyVarUnique u87
      => Expr TyAttr:
      TyAttr - TyVar t27 <> TyVarUnique u87
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Free 2
      Free Expr
          Expr: UnboxedSingleton
              Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t27 <> TyVarUnique u87
      => Expr TyAttr:
      TyAttr - TyVar t27 <> TyVarUnique u87
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: Free 2 - TyAttr - TyVar t27 <> TyVarUnique u87
      Free Expr
          Pretyped Expr: UnboxedSingleton - TyAttr - TyVar t27 <> TyVarUnique u87
              Value: Var: x - TyAttr - TyVar t27 <> TyVarUnique u87 |}]

let%expect_test "Constraints Generation Expr: Weak" =
  let expr_weak =
    Weak
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (fresh (), fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_weak ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyVar t28 <> TyVarUnique u88
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t28 <> TyVarUnique u88
      => Expr TyAttr:
      TyAttr - TyVar t28 <> TyVarUnique u88
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Weak 2
      Weak Expr
          Expr: UnboxedSingleton
              Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t28 <> TyVarUnique u88
      => Expr TyAttr:
      TyAttr - TyVar t28 <> TyVarUnique u88
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: Weak 2 - TyAttr - TyVar t28 <> TyVarUnique u88
      Weak Expr
          Pretyped Expr: UnboxedSingleton - TyAttr - TyVar t28 <> TyVarUnique u88
              Value: Var: x - TyAttr - TyVar t28 <> TyVarUnique u88 |}]

let%expect_test "Constraints Generation Expr: Inst" =
  let expr_inst =
    Inst
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", (fresh (), fresh_unique ()));
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_inst ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
      Actual value:
      Value: Var: x
      => Value Ty:
      TyAttr - TyVar t29 <> TyVarUnique u89
      => Value Constraints:
      => Value Uniqueness Constraints:
      -------------------------

      Actual expr:
      Expr: UnboxedSingleton
          Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t29 <> TyVarUnique u89
      => Expr TyAttr:
      TyAttr - TyVar t29 <> TyVarUnique u89
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Actual expr:
      Expr: Inst 2
      Inst Expr
          Expr: UnboxedSingleton
              Value: Var: x

      => Typing Context:
      x : TyAttr - TyVar t29 <> TyVarUnique u89
      => Expr TyAttr:
      TyAttr - TyVar t29 <> TyVarUnique u89
      => Expr Constraints:
      => Expr Unique Constraints;
      -------------------------

      Pretyped Expr: Inst 2 - TyAttr - TyVar t29 <> TyVarUnique u89
      Inst Expr
          Pretyped Expr: UnboxedSingleton - TyAttr - TyVar t29 <> TyVarUnique u89
              Value: Var: x - TyAttr - TyVar t29 <> TyVarUnique u89 |}]
