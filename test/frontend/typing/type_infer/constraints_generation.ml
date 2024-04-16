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
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Unit
    => Value Ty:
    TyUnit
    => Value Constraints:
    -------------------------

    Value: Unit - TyUnit |}]

let%expect_test "Constraints Generation Value: Int" =
  let value_int = Integer (mock_loc, 0) in
  (match
     generate_constraints_value_expr [] [] [] [] value_int ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Int: 0
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Value: Int: 0 - TyInt |}]

let%expect_test "Constraints Generation Value: Atom Constructor" =
  let value_atom =
    Constructor (mock_loc, Constructor_name.of_string "AtomConstructor", [])
  in
  let types_env =
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
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
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Constructor: AtomConstructor
        ()
    => Value Ty:
    TyVar t1
    => Value Constraints:
    (TyVar t1, TyCustom () custom_type)
    -------------------------

    Value: Constructor: AtomConstructor - TyVar t1
        () |}]

let%expect_test "Constraints Generation Value: Complex Constructor" =
  let value_constructor =
    Constructor
      ( mock_loc,
        Constructor_name.of_string "Constructor",
        [ Integer (mock_loc, 0) ] )
  in
  let types_env =
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "Constructor",
          [ TEInt mock_loc ] );
    ]
  in
  (match
     generate_constraints_value_expr types_env constructors_env [] []
       value_constructor ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Int: 0
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Constructor: Constructor
        ConstructorArg
            Value: Int: 0
    => Value Ty:
    TyVar t2
    => Value Constraints:
    (TyVar t2, TyCustom () custom_type)
    (TyInt, TyInt)
    -------------------------

    Value: Constructor: Constructor - TyVar t2
            ConstructorArg
            Value: Int: 0 - TyInt |}]

let%expect_test "Constraints Generation Value: Polymorphic Complex Constructor"
    =
  let value_constructor =
    Constructor
      ( mock_loc,
        Constructor_name.of_string "Constructor",
        [
          Unit mock_loc;
          Integer (mock_loc, 1);
          Constructor (mock_loc, Constructor_name.of_string "Atom", []);
        ] )
  in
  let types_env =
    [
      Type_defns_env.TypesEnvEntry
        ( [ TEPoly (mock_loc, "'a"); TEPoly (mock_loc, "'b") ],
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
            TEPoly (mock_loc, "'b");
            TEInt mock_loc;
            TECustom
              ( mock_loc,
                [ TEBool mock_loc; TEPoly (mock_loc, "'a") ],
                Type_name.of_string "custom_type" );
          ] );
    ]
  in
  (match
     generate_constraints_value_expr types_env constructors_env [] []
       value_constructor ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Constructor: Atom
        ()
    => Value Ty:
    TyVar t6
    => Value Constraints:
    (TyVar t6, TyCustom (TyVar t7, TyVar t8) custom_type)
    -------------------------

    Actual value:
    Value: Int: 1
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Unit
    => Value Ty:
    TyUnit
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Constructor: Constructor
        ConstructorArg
            Value: Unit
        ConstructorArg
            Value: Int: 1
        ConstructorArg
            Value: Constructor: Atom
                ()
    => Value Ty:
    TyVar t3
    => Value Constraints:
    (TyVar t3, TyCustom (TyVar t4, TyVar t5) custom_type)
    (TyUnit, TyVar t5)
    (TyInt, TyInt)
    (TyVar t6, TyCustom (TyBool, TyVar t4) custom_type)
    (TyVar t6, TyCustom (TyVar t7, TyVar t8) custom_type)
    -------------------------

    Value: Constructor: Constructor - TyVar t3
            ConstructorArg
            Value: Unit - TyUnit
            ConstructorArg
            Value: Int: 1 - TyInt
            ConstructorArg
            Value: Constructor: Atom - TyVar t6
                () |}]

let%expect_test "Constraints Generation Value: Variable" =
  let value_var = Variable (mock_loc, Var_name.of_string "x") in
  let typing_context =
    [ Type_context_env.TypingContextEntry (Var_name.of_string "x", TyInt) ]
  in
  (match
     generate_constraints_value_expr [] [] [] typing_context value_var
       ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, pretyped_value) ->
      Pprint_pretyped_ast.pprint_pretyped_value Fmt.stdout ~indent:""
        pretyped_value);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Value: Var: x - TyInt |}]

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
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [ TEBool mock_loc; TEInt mock_loc ] );
    ]
  in
  let typing_context =
    [ Type_context_env.TypingContextEntry (Var_name.of_string "x", TyInt) ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_singleton ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Bool: true
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Constructor: C
        ConstructorArg
            Value: Bool: true
        ConstructorArg
            Value: Var: x
    => Value Ty:
    TyVar t9
    => Value Constraints:
    (TyVar t9, TyCustom () custom_type)
    (TyBool, TyBool)
    (TyInt, TyInt)
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Constructor: C
            ConstructorArg
                Value: Bool: true
            ConstructorArg
                Value: Var: x

    => Typing Context:
    x : TyInt
    => Expr Ty:
    TyVar t9
    => Expr Constraints:
    (TyVar t9, TyCustom () custom_type)
    (TyBool, TyBool)
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: UnboxedSingleton - TyVar t9
        Value: Constructor: C - TyVar t9
                ConstructorArg
                Value: Bool: true - TyBool
                ConstructorArg
                Value: Var: x - TyInt |}]

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
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [ TEBool mock_loc ] );
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry (Var_name.of_string "x", TyBool);
      Type_context_env.TypingContextEntry (Var_name.of_string "y", TyInt);
    ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_tuple ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: y
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: x
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Constructor: C
        ConstructorArg
            Value: Var: x
    => Value Ty:
    TyVar t10
    => Value Constraints:
    (TyVar t10, TyCustom () custom_type)
    (TyBool, TyBool)
    -------------------------

    Actual value:
    Value: Bool: true
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Unit
    => Value Ty:
    TyUnit
    => Value Constraints:
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
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyTuple (TyUnit, TyBool, TyVar t10, TyInt)
    => Expr Constraints:
    (TyVar t10, TyCustom () custom_type)
    (TyBool, TyBool)
    -------------------------

    Pretyped Expr: UnboxedTuple - TyTuple (TyUnit, TyBool, TyVar t10, TyInt)
        Value: Unit - TyUnit
        Value: Bool: true - TyBool
        Value: Constructor: C - TyVar t10
                ConstructorArg
                Value: Var: x - TyBool
        Value: Var: y - TyInt |}]

let%expect_test "Constraints Generation Expr: Let" =
  let expr_let =
    Let
      ( mock_loc,
        [ Var_name.of_string "x"; Var_name.of_string "y" ],
        UnboxedTuple
          (mock_loc, [ Integer (mock_loc, 0); Boolean (mock_loc, true) ]),
        UnboxedTuple
          ( mock_loc,
            [
              Variable (mock_loc, Var_name.of_string "x");
              Variable (mock_loc, Var_name.of_string "y");
            ] ) )
  in
  (match generate_constraints [] [] [] [] expr_let ~verbose:true with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Bool: true
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Int: 0
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Int: 0
        Value: Bool: true

    => Typing Context:
    => Expr Ty:
    TyTuple (TyInt, TyBool)
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Var: y
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: x
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Var: x
        Value: Var: y

    => Typing Context:
    y : TyPoly - for all (). TyBool
    x : TyPoly - for all (). TyInt
    => Expr Ty:
    TyTuple (TyInt, TyBool)
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Let vars: (x, y) =
        Expr: UnboxedTuple
            Value: Int: 0
            Value: Bool: true
        Expr: UnboxedTuple
            Value: Var: x
            Value: Var: y

    => Typing Context:
    y : TyPoly - for all (). TyBool
    x : TyPoly - for all (). TyInt
    => Expr Ty:
    TyTuple (TyInt, TyBool)
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Let vars: (x, y) =
        Pretyped Expr: UnboxedTuple - TyTuple (TyInt, TyBool)
            Value: Int: 0 - TyInt
            Value: Bool: true - TyBool
    Pretyped Expr: Let expr - TyTuple (TyInt, TyBool)
        Pretyped Expr: UnboxedTuple - TyTuple (TyInt, TyBool)
            Value: Var: x - TyInt
            Value: Var: y - TyBool |}]

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
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "x",
          TyCustom ([], Type_name.of_string "custom_type") );
      Type_context_env.TypingContextEntry
        (Var_name.of_string "bool_to_bool_fun", TyArrow (TyBool, TyBool));
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "f",
          TyArrow
            ( TyArrow (TyBool, TyBool),
              TyArrow
                ( TyCustom ([], Type_name.of_string "custom_type"),
                  TyTuple
                    [
                      TyInt;
                      TyBool;
                      TyCustom ([], Type_name.of_string "custom_type");
                    ] ) ) );
    ]
  in
  (match
     generate_constraints types_env [] [] typing_context expr_funapp
       ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyCustom () custom_type
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: bool_to_bool_fun
    => Value Ty:
    TyArrow (TyBool -> TyBool)
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: FunApp
        FunctionVar: f
        FunApp Args:
            Value: Var: bool_to_bool_fun
            Value: Var: x

    => Typing Context:
    x : TyCustom () custom_type
    bool_to_bool_fun : TyArrow (TyBool -> TyBool)
    f : TyArrow (TyArrow (TyBool -> TyBool) -> TyArrow (TyCustom () custom_type -> TyTuple (TyInt, TyBool, TyCustom () custom_type)))
    => Expr Ty:
    TyTuple (TyInt, TyBool, TyCustom () custom_type)
    => Expr Constraints:
    (TyArrow (TyBool -> TyBool), TyArrow (TyBool -> TyBool))
    (TyCustom () custom_type, TyCustom () custom_type)
    -------------------------

    Pretyped Expr: FunApp - TyTuple (TyInt, TyBool, TyCustom () custom_type)
        FunctionVar: f
        FunctionArg
            Value: Var: bool_to_bool_fun - TyArrow (TyBool -> TyBool)
        FunctionArg
            Value: Var: x - TyCustom () custom_type |}]

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
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C",
          [ TEUnit mock_loc ] );
    ]
  in
  let functions_env =
    [
      Functions_env.FunctionEnvEntry
        ( 1,
          None,
          Function_name.of_string "mock_fun",
          [
            TEInt mock_loc;
            TECustom (mock_loc, [], Type_name.of_string "custom_type");
          ],
          TETuple
            ( mock_loc,
              [
                TEInt mock_loc;
                TECustom (mock_loc, [], Type_name.of_string "custom_type");
              ] ) );
    ]
  in
  (match
     generate_constraints types_env constructors_env functions_env []
       expr_funcall ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Unit
    => Value Ty:
    TyUnit
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Constructor: C
        ConstructorArg
            Value: Unit
    => Value Ty:
    TyVar t11
    => Value Constraints:
    (TyVar t11, TyCustom () custom_type)
    (TyUnit, TyUnit)
    -------------------------

    Actual value:
    Value: Int: 0
    => Value Ty:
    TyInt
    => Value Constraints:
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
    => Expr Ty:
    TyTuple (TyInt, TyCustom () custom_type)
    => Expr Constraints:
    (TyInt, TyInt)
    (TyCustom () custom_type, TyVar t11)
    (TyVar t11, TyCustom () custom_type)
    (TyUnit, TyUnit)
    -------------------------

    Pretyped Expr: FunCall - TyTuple (TyInt, TyCustom () custom_type)
        Function Name: mock_fun
        FunctionArg
            Value: Int: 0 - TyInt
        FunctionArg
            Value: Constructor: C - TyVar t11
                    ConstructorArg
                    Value: Unit - TyUnit |}]

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
      Type_context_env.TypingContextEntry (Var_name.of_string "x", TyBool);
      Type_context_env.TypingContextEntry (Var_name.of_string "y", TyInt);
    ]
  in
  (match generate_constraints [] [] [] typing_context expr_if ~verbose:true with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Var: y
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: x
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Var: x
        Value: Var: y

    => Typing Context:
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyTuple (TyBool, TyInt)
    => Expr Constraints:
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
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyVar t12
    => Expr Constraints:
    (TyBool, TyBool)
    (TyVar t12, TyTuple (TyBool, TyInt))
    -------------------------

    Pretyped Expr: If - TyVar t12
        Pretyped Expr: UnboxedSingleton - TyBool
            Value: Var: x - TyBool
    Then
        Pretyped Expr: UnboxedTuple - TyTuple (TyBool, TyInt)
            Value: Var: x - TyBool
            Value: Var: y - TyInt |}]

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
      Type_context_env.TypingContextEntry (Var_name.of_string "x", TyBool);
      Type_context_env.TypingContextEntry (Var_name.of_string "y", TyInt);
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_if_else ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Var: y
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: x
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Var: x
        Value: Var: y

    => Typing Context:
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyTuple (TyBool, TyInt)
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Int: 1
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Bool: false
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Bool: false
        Value: Int: 1

    => Typing Context:
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyTuple (TyBool, TyInt)
    => Expr Constraints:
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
    x : TyBool
    y : TyInt
    => Expr Ty:
    TyVar t13
    => Expr Constraints:
    (TyBool, TyBool)
    (TyVar t13, TyTuple (TyBool, TyInt))
    (TyVar t13, TyTuple (TyBool, TyInt))
    -------------------------

    Pretyped Expr: IfElse - TyVar t13
        Pretyped Expr: UnboxedSingleton - TyBool
            Value: Var: x - TyBool
    Then
        Pretyped Expr: UnboxedTuple - TyTuple (TyBool, TyInt)
            Value: Var: x - TyBool
            Value: Var: y - TyInt
    Else
        Pretyped Expr: UnboxedTuple - TyTuple (TyBool, TyInt)
            Value: Bool: false - TyBool
            Value: Int: 1 - TyInt |}]

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
    [ Type_defns_env.TypesEnvEntry ([], Type_name.of_string "custom_type") ]
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C1",
          [ TEInt mock_loc ] );
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C2",
          [
            TEBool mock_loc;
            TECustom (mock_loc, [], Type_name.of_string "custom_type");
          ] );
    ]
  in
  let typing_context =
    [
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "x",
          TyCustom ([], Type_name.of_string "custom_type") );
    ]
  in
  (match
     generate_constraints types_env constructors_env [] typing_context
       expr_match ~verbose:true
   with
  | Error err -> Fmt.pf Fmt.stdout "%s" (Error.to_string_hum err)
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: y
    => Value Ty:
    TyVar t16
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Int: 1
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Int: 1
        Value: Var: y

    => Typing Context:
    y : TyVar t16
    x : TyCustom () custom_type
    => Expr Ty:
    TyTuple (TyInt, TyVar t16)
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Bool: true
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Var: y
    => Value Ty:
    TyVar t17
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Var: y
        Value: Bool: true

    => Typing Context:
    y : TyVar t17
    x : TyCustom () custom_type
    => Expr Ty:
    TyTuple (TyVar t17, TyBool)
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Bool: true
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual value:
    Value: Int: 0
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedTuple
        Value: Int: 0
        Value: Bool: true

    => Typing Context:
    x : TyCustom () custom_type
    => Expr Ty:
    TyTuple (TyInt, TyBool)
    => Expr Constraints:
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
    x : TyCustom () custom_type
    => Expr Ty:
    TyVar t14
    => Expr Constraints:
    (TyCustom () custom_type, TyVar t18)
    (TyVar t14, TyTuple (TyInt, TyBool))
    (TyCustom () custom_type, TyCustom () custom_type)
    (TyVar t14, TyTuple (TyVar t17, TyBool))
    (TyVar t17, TyInt)
    (TyCustom () custom_type, TyCustom () custom_type)
    (TyVar t14, TyTuple (TyInt, TyVar t16))
    (TyVar t16, TyBool)
    (TyCustom () custom_type, TyCustom () custom_type)
    (TyVar t15, TyInt)
    -------------------------

    Pretyped Expr: Match - TyVar t14
        Match Var: x
        PatternExpr - TyTuple (TyInt, TyBool)
            Pretyped MatchedExpr - TyVar t18: Underscore
        PatternMatchExpr
            Pretyped Expr: UnboxedTuple - TyTuple (TyInt, TyBool)
                Value: Int: 0 - TyInt
                Value: Bool: true - TyBool
        PatternExpr - TyTuple (TyVar t17, TyBool)
            Pretyped MatchedExpr - TyCustom () custom_type: C1
                Pretyped MatchedExpr - TyVar t17: Var y
        PatternMatchExpr
            Pretyped Expr: UnboxedTuple - TyTuple (TyVar t17, TyBool)
                Value: Var: y - TyVar t17
                Value: Bool: true - TyBool
        PatternExpr - TyTuple (TyInt, TyVar t16)
            Pretyped MatchedExpr - TyCustom () custom_type: C2
                Pretyped MatchedExpr - TyVar t16: Var y
                Pretyped MatchedExpr - TyCustom () custom_type: C1
                    Pretyped MatchedExpr - TyVar t15: Underscore
        PatternMatchExpr
            Pretyped Expr: UnboxedTuple - TyTuple (TyInt, TyVar t16)
                Value: Int: 1 - TyInt
                Value: Var: y - TyVar t16 |}]

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
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyInt);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", Type_infer_types.TyBool);
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_neg ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  Fmt.pf Fmt.stdout "\n";
  (match
     generate_constraints [] [] [] typing_context expr_not ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Unary Op: -
        Expr: UnboxedSingleton
            Value: Var: x

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyInt
    => Expr Constraints:
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: - - TyInt
        Pretyped Expr: UnboxedSingleton - TyInt
            Value: Var: x - TyInt

    Actual value:
    Value: Var: y
    => Value Ty:
    TyBool
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: y

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Unary Op: !
        Expr: UnboxedSingleton
            Value: Var: y

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyBool, TyBool)
    -------------------------

    Pretyped Expr: ! - TyBool
        Pretyped Expr: UnboxedSingleton - TyBool
            Value: Var: y - TyBool |}]

let%expect_test "Constraints Generation Expr: BinOp" =
  let typing_context_ints : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry (Var_name.of_string "x1", TyInt);
      Type_context_env.TypingContextEntry (Var_name.of_string "x2", TyInt);
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
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x1
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x1

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual value:
    Value: Var: x2
    => Value Ty:
    TyInt
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x2

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
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
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    (TyInt, TyInt)
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: + - TyInt
        Pretyped Expr: UnboxedSingleton - TyInt
            Value: Var: x1 - TyInt
        Pretyped Expr: UnboxedSingleton - TyInt
            Value: Var: x2 - TyInt |}]

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
      Type_context_env.TypingContextEntry (Var_name.of_string "x", TyInt);
      Type_context_env.TypingContextEntry (Var_name.of_string "y", fresh ());
    ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_drop ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: y
    => Value Ty:
    TyVar t19
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: y

    => Typing Context:
    y : TyVar t19
    => Expr Ty:
    TyVar t19
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Drop x - Expr:
        Expr: UnboxedSingleton
            Value: Var: y

    => Typing Context:
    x : TyInt
    y : TyVar t19
    => Expr Ty:
    TyVar t19
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Drop x - TyUnit Expr:
        Pretyped Expr: UnboxedSingleton - TyVar t19
            Value: Var: y - TyVar t19 |}]

let%expect_test "Constraints Generation Expr: Free" =
  let expr_free =
    Free
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [ Type_context_env.TypingContextEntry (Var_name.of_string "x", fresh ()) ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_free ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyVar t20
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyVar t20
    => Expr Ty:
    TyVar t20
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Free 2
    Free Expr
        Expr: UnboxedSingleton
            Value: Var: x

    => Typing Context:
    x : TyVar t20
    => Expr Ty:
    TyVar t20
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Free 2 - TyVar t20
    Free Expr
        Pretyped Expr: UnboxedSingleton - TyVar t20
            Value: Var: x - TyVar t20 |}]

let%expect_test "Constraints Generation Expr: Weak" =
  let expr_weak =
    Weak
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [ Type_context_env.TypingContextEntry (Var_name.of_string "x", fresh ()) ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_weak ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyVar t21
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyVar t21
    => Expr Ty:
    TyVar t21
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Weak 2
    Weak Expr
        Expr: UnboxedSingleton
            Value: Var: x

    => Typing Context:
    x : TyVar t21
    => Expr Ty:
    TyVar t21
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Weak 2 - TyVar t21
    Weak Expr
        Pretyped Expr: UnboxedSingleton - TyVar t21
            Value: Var: x - TyVar t21 |}]

let%expect_test "Constraints Generation Expr: Inst" =
  let expr_inst =
    Inst
      ( mock_loc,
        2,
        UnboxedSingleton (mock_loc, Variable (mock_loc, Var_name.of_string "x"))
      )
  in
  let typing_context =
    [ Type_context_env.TypingContextEntry (Var_name.of_string "x", fresh ()) ]
  in
  (match
     generate_constraints [] [] [] typing_context expr_inst ~verbose:true
   with
  | Error _ -> ()
  | Ok (_, _, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr);
  [%expect
    {|
    Actual value:
    Value: Var: x
    => Value Ty:
    TyVar t22
    => Value Constraints:
    -------------------------

    Actual expr:
    Expr: UnboxedSingleton
        Value: Var: x

    => Typing Context:
    x : TyVar t22
    => Expr Ty:
    TyVar t22
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Inst 2
    Inst Expr
        Expr: UnboxedSingleton
            Value: Var: x

    => Typing Context:
    x : TyVar t22
    => Expr Ty:
    TyVar t22
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Inst 2 - TyVar t22
    Inst Expr
        Pretyped Expr: UnboxedSingleton - TyVar t22
            Value: Var: x - TyVar t22 |}]
