open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "Constraints Generation: Basic Expr - Unit" =
  let expr = Unit mock_loc in
  let open Result in
  match
    Type_infer_constraints_generator.generate_constraints [] [] [] expr
      ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
        Actual expr:
        Expr: Unit

        => Typing Context:
        => Expr Ty:
        TyUnit
        => Expr Constraints:
        -------------------------

        Pretyped Expr: Unit - TyUnit |}]

let%expect_test "Constraints Generation: Option Expr - Unit" =
  let expr = Option (mock_loc, Some (Unit mock_loc)) in
  match
    Type_infer_constraints_generator.generate_constraints [] [] [] expr
      ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Unit

    => Typing Context:
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Option - Some
        Expr: Unit

    => Typing Context:
    => Expr Ty:
    TyVar t1
    => Expr Constraints:
    (TyVar t1, TyOption TyUnit)
    -------------------------

    Pretyped Expr: Option Some - TyVar t1
        Pretyped Expr: Unit - TyUnit |}]

let%expect_test "Constraints Generation: Option Variable" =
  let expr =
    Option (mock_loc, Some (Variable (mock_loc, Var_name.of_string "x")))
  in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyInt);
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints [] [] typing_context
      expr ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Option - Some
        Expr: Var: x

    => Typing Context:
    x : TyInt
    => Expr Ty:
    TyVar t2
    => Expr Constraints:
    (TyVar t2, TyOption TyInt)
    -------------------------

    Pretyped Expr: Option Some - TyVar t2
        Pretyped Expr: Var x - TyInt |}]

let%expect_test "Constraints Generation: Constructor" =
  let expr_arg = Variable (mock_loc, Var_name.of_string "x") in
  let expr =
    Constructor (mock_loc, Constructor_name.of_string "C1", [ expr_arg ])
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C1",
          [ TEInt mock_loc ] );
    ]
  in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyInt);
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints constructors_env []
      typing_context expr ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Constructor: C1
        ConstructorArg
            Expr: Var: x

    => Typing Context:
    x : TyInt
    => Expr Ty:
    TyVar t3
    => Expr Constraints:
    (TyVar t3, TyCustom custom_type)
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: Constructor C1 - TyVar t3
        ConstructorArg
            Pretyped Expr: Var x - TyInt |}]

let%expect_test "Constraints Generation: Let Expr" =
  let in_expr = Variable (mock_loc, Var_name.of_string "x") in
  let var_expr = Option (mock_loc, None) in
  let expr = Let (mock_loc, Var_name.of_string "x", var_expr, in_expr) in
  match
    Type_infer_constraints_generator.generate_constraints [] [] [] expr
      ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Option - None

    => Typing Context:
    => Expr Ty:
    TyVar t4
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyVar t4
    => Expr Ty:
    TyVar t4
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Let var: x =
        Expr: Option - None
        Expr: Var: x

    => Typing Context:
    => Expr Ty:
    TyVar t4
    => Expr Constraints:
    -------------------------

    Pretyped Expr: Let var x - TyVar t4
        Pretyped Expr: Option None - TyVar t4
    Pretyped Expr: Let Expr - TyVar t4
        Pretyped Expr: Var x - TyVar t4 |}]

let%expect_test "Constraints Generation: If & IfElse Expr" =
  let expr_cond = Variable (mock_loc, Var_name.of_string "x") in
  let expr_then =
    Block
      (mock_loc, [ Unit mock_loc; Variable (mock_loc, Var_name.of_string "y") ])
  in
  let expr_else =
    Block
      ( mock_loc,
        [
          Variable (mock_loc, Var_name.of_string "u");
          Variable (mock_loc, Var_name.of_string "z");
        ] )
  in
  let expr = IfElse (mock_loc, expr_cond, expr_then, expr_else) in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyBool);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", Type_infer_types.TyInt);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "z", Type_infer_types.TyInt);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "u", Type_infer_types.TyUnit);
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints [] [] typing_context
      expr ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Unit

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: y

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Unit
        Expr: Var: y

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Block Expr Ty:
    TyInt
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: Var: u

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: z

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Var: u
        Expr: Var: z

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Block Expr Ty:
    TyInt
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: IfElse
        Expr: Var: x
        Then Block
            Expr: Unit
            Expr: Var: y
        Else Block
            Expr: Var: u
            Expr: Var: z

    => Typing Context:
    x : TyBool
    y : TyInt
    z : TyInt
    u : TyUnit
    => Expr Ty:
    TyVar t5
    => Expr Constraints:
    (TyBool, TyBool)
    (TyVar t5, TyInt)
    (TyVar t5, TyInt)
    (TyUnit, TyUnit)
    (TyUnit, TyUnit)
    -------------------------

    Pretyped Expr: IfElse - TyVar t5
        Pretyped Expr: Var x - TyBool
        Pretyped Then Block Expr Type - TyInt
            Pretyped Expr: Unit - TyUnit
            Pretyped Expr: Var y - TyInt
        Pretyped Else Block Expr Type - TyInt
            Pretyped Expr: Var u - TyUnit
            Pretyped Expr: Var z - TyInt |}]

let%expect_test "Constraints Generation: UnOp Expr - UnOpNot, UnOpNeg" =
  let expr_neg =
    UnOp (mock_loc, UnOpNeg, Variable (mock_loc, Var_name.of_string "x"))
  in
  let expr_not =
    UnOp (mock_loc, UnOpNot, Variable (mock_loc, Var_name.of_string "y"))
  in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyInt);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", Type_infer_types.TyBool);
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints [] [] typing_context
      expr_neg ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Unary Op: -
        Expr: Var: x

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyInt
    => Expr Constraints:
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: - - TyInt
        Pretyped Expr: Var x - TyInt |}];
      ignore
        (Type_infer_constraints_generator.generate_constraints [] []
           typing_context expr_not ~verbose:true);
      [%expect
        {|
    Actual expr:
    Expr: Var: y

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Unary Op: !
        Expr: Var: y

    => Typing Context:
    x : TyInt
    y : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyBool, TyBool)
    ------------------------- |}]

let%expect_test "Constraints Generation: BinOp Expr - BinOpPlus, BinOpLt, \
                 BinOpEq, BinOpAnd" =
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
        Variable (mock_loc, Var_name.of_string "x1"),
        Variable (mock_loc, Var_name.of_string "x2") )
  in
  match
    Type_infer_constraints_generator.generate_constraints [] []
      typing_context_ints bin_op_plus_expr ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) -> (
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x1

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: x2

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
        Expr: Var: x1
        RightExpr
        Expr: Var: x2

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
        Pretyped Expr: Var x1 - TyInt
        Pretyped Expr: Var x2 - TyInt |}];
      let bin_op_lt_expr =
        BinaryOp
          ( mock_loc,
            BinOpLt,
            Variable (mock_loc, Var_name.of_string "x1"),
            Variable (mock_loc, Var_name.of_string "x2") )
      in
      match
        Type_infer_constraints_generator.generate_constraints [] []
          typing_context_ints bin_op_lt_expr ~verbose:true
      with
      | Error _ -> ()
      | Ok (_, _, _, pretyped_expr) -> (
          Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
            pretyped_expr;
          [%expect
            {|
    Actual expr:
    Expr: Var: x1

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: x2

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Binary Op: <
        LeftExpr
        Expr: Var: x1
        RightExpr
        Expr: Var: x2

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyInt, TyInt)
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: < - TyBool
        Pretyped Expr: Var x1 - TyInt
        Pretyped Expr: Var x2 - TyInt |}];

          let typing_context_bools : Type_infer_types.typing_context =
            [
              Type_context_env.TypingContextEntry
                (Var_name.of_string "b1", TyBool);
              Type_context_env.TypingContextEntry
                (Var_name.of_string "b2", TyBool);
            ]
          in
          let bin_op_and_expr =
            BinaryOp
              ( mock_loc,
                BinOpAnd,
                Variable (mock_loc, Var_name.of_string "b1"),
                Variable (mock_loc, Var_name.of_string "b2") )
          in
          match
            Type_infer_constraints_generator.generate_constraints [] []
              typing_context_bools bin_op_and_expr ~verbose:true
          with
          | Error _ -> ()
          | Ok (_, _, _, pretyped_expr) -> (
              Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
                pretyped_expr;
              [%expect
                {|
    Actual expr:
    Expr: Var: b1

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: b2

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Binary Op: &&
        LeftExpr
        Expr: Var: b1
        RightExpr
        Expr: Var: b2

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyBool, TyBool)
    (TyBool, TyBool)
    -------------------------

    Pretyped Expr: && - TyBool
        Pretyped Expr: Var b1 - TyBool
        Pretyped Expr: Var b2 - TyBool |}];
              let bin_op_eq_expr_ints =
                BinaryOp
                  ( mock_loc,
                    BinOpEq,
                    Variable (mock_loc, Var_name.of_string "x1"),
                    Variable (mock_loc, Var_name.of_string "x2") )
              in
              match
                Type_infer_constraints_generator.generate_constraints [] []
                  typing_context_ints bin_op_eq_expr_ints ~verbose:true
              with
              | Error _ -> ()
              | Ok (_, _, _, pretyped_expr) -> (
                  Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
                    pretyped_expr;
                  [%expect
                    {|
    Actual expr:
    Expr: Var: x1

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: x2

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Binary Op: ==
        LeftExpr
        Expr: Var: x1
        RightExpr
        Expr: Var: x2

    => Typing Context:
    x1 : TyInt
    x2 : TyInt
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: == - TyBool
        Pretyped Expr: Var x1 - TyInt
        Pretyped Expr: Var x2 - TyInt |}];
                  let bin_op_eq_expr_bools =
                    BinaryOp
                      ( mock_loc,
                        BinOpEq,
                        Variable (mock_loc, Var_name.of_string "b1"),
                        Variable (mock_loc, Var_name.of_string "b2") )
                  in
                  match
                    Type_infer_constraints_generator.generate_constraints [] []
                      typing_context_bools bin_op_eq_expr_bools ~verbose:true
                  with
                  | Error _ -> ()
                  | Ok (_, _, _, pretyped_expr) ->
                      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout
                        ~indent:"" pretyped_expr;
                      [%expect
                        {|
    Actual expr:
    Expr: Var: b1

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: b2

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Actual expr:
    Binary Op: ==
        LeftExpr
        Expr: Var: b1
        RightExpr
        Expr: Var: b2

    => Typing Context:
    b1 : TyBool
    b2 : TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    (TyBool, TyBool)
    -------------------------

    Pretyped Expr: == - TyBool
        Pretyped Expr: Var b1 - TyBool
        Pretyped Expr: Var b2 - TyBool |}]
                  ))))

let%expect_test "Constraints Generation: FunApp Expr" =
  let functions_env : Functions_env.functions_env =
    [
      Functions_env.FunctionEnvEntry
        ( Function_name.of_string "f",
          [
            TEInt mock_loc;
            TEUnit mock_loc;
            TECustom (mock_loc, Type_name.of_string "custom_type");
          ],
          TEInt mock_loc );
    ]
  in
  let fun_app_expr =
    FunApp
      ( mock_loc,
        Function_name.of_string "f",
        [
          Variable (mock_loc, Var_name.of_string "x");
          Variable (mock_loc, Var_name.of_string "y");
          Constructor
            ( mock_loc,
              Constructor_name.of_string "C1",
              [ Variable (mock_loc, Var_name.of_string "z") ] );
        ] )
  in
  let constructors_env =
    [
      Type_defns_env.ConstructorEnvEntry
        ( Type_name.of_string "custom_type",
          Constructor_name.of_string "C1",
          [ TEInt mock_loc ] );
    ]
  in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        (Var_name.of_string "x", Type_infer_types.TyInt);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "y", Type_infer_types.TyUnit);
      Type_context_env.TypingContextEntry
        (Var_name.of_string "z", Type_infer_types.TyInt);
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints constructors_env
      functions_env typing_context fun_app_expr ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) ->
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyInt
    y : TyUnit
    z : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: y

    => Typing Context:
    x : TyInt
    y : TyUnit
    z : TyInt
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: z

    => Typing Context:
    x : TyInt
    y : TyUnit
    z : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Constructor: C1
        ConstructorArg
            Expr: Var: z

    => Typing Context:
    x : TyInt
    y : TyUnit
    z : TyInt
    => Expr Ty:
    TyVar t6
    => Expr Constraints:
    (TyVar t6, TyCustom custom_type)
    (TyInt, TyInt)
    -------------------------

    Actual expr:
    Expr: FunApp
        Function: f
        FunctionArg
            Expr: Var: x
        FunctionArg
            Expr: Var: y
        FunctionArg
            Expr: Constructor: C1
                ConstructorArg
                    Expr: Var: z

    => Typing Context:
    x : TyInt
    y : TyUnit
    z : TyInt
    => Expr Ty:
    TyInt
    => Expr Constraints:
    (TyCustom custom_type, TyVar t6)
    (TyVar t6, TyCustom custom_type)
    (TyInt, TyInt)
    (TyUnit, TyUnit)
    (TyInt, TyInt)
    -------------------------

    Pretyped Expr: FunApp - TyInt
        Function: f
        FunctionArg
            Pretyped Expr: Var x - TyInt
        FunctionArg
            Pretyped Expr: Var y - TyUnit
        FunctionArg
            Pretyped Expr: Constructor C1 - TyVar t6
                ConstructorArg
                    Pretyped Expr: Var z - TyInt |}]

let%expect_test "Constraints Generation: Match & DMatch Expr" =
  let expr_optional =
    Match
      ( mock_loc,
        Var_name.of_string "x",
        [
          Parsing.Parser_ast.MPattern
            ( mock_loc,
              MOption
                ( mock_loc,
                  Some
                    (Parsing.Parser_ast.MVariable
                       (mock_loc, Var_name.of_string "y")) ),
              Block (mock_loc, [ Variable (mock_loc, Var_name.of_string "y") ])
            );
          Parsing.Parser_ast.MPattern
            ( mock_loc,
              MOption (mock_loc, None),
              Block (mock_loc, [ Unit mock_loc; Boolean (mock_loc, true) ]) );
        ] )
  in
  let typing_context : Type_infer_types.typing_context =
    [
      Type_context_env.TypingContextEntry
        ( Var_name.of_string "x",
          Type_infer_types.TyOption Type_infer_types.TyBool );
    ]
  in
  match
    Type_infer_constraints_generator.generate_constraints [] [] typing_context
      expr_optional ~verbose:true
  with
  | Error _ -> ()
  | Ok (_, _, _, pretyped_expr) -> (
      Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
        pretyped_expr;
      [%expect
        {|
    Actual expr:
    Expr: Var: y

    => Typing Context:
    y : TyVar t9
    x : TyOption TyBool
    => Expr Ty:
    TyVar t9
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Var: y

    => Typing Context:
    y : TyVar t9
    x : TyOption TyBool
    => Block Expr Ty:
    TyVar t9
    => Block Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Unit

    => Typing Context:
    x : TyOption TyBool
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Bool: true

    => Typing Context:
    x : TyOption TyBool
    => Expr Ty:
    TyBool
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Unit
        Expr: Bool: true

    => Typing Context:
    x : TyOption TyBool
    => Block Expr Ty:
    TyBool
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: Match
        Match Var: x
        PatternExpr
            MatchedExpr: MOption - Some
                MatchedExpr: Var - y
            PatternBlockExpr Block
                Expr: Var: y
        PatternExpr
            MatchedExpr: MOption - None
            PatternBlockExpr Block
                Expr: Unit
                Expr: Bool: true

    => Typing Context:
    x : TyOption TyBool
    => Expr Ty:
    TyVar t7
    => Expr Constraints:
    (TyOption TyBool, TyVar t10)
    (TyVar t7, TyBool)
    (TyUnit, TyUnit)
    (TyOption TyBool, TyVar t8)
    (TyVar t7, TyVar t9)
    (TyVar t8, TyOption TyVar t9)
    -------------------------

    Pretyped Expr: Match - TyVar t7
        Match Var: x
        PatternExpr - TyBool
            Pretyped MatchedExpr - TyVar t10: Option None
            Pretyped PatternBlockExpr Block Expr Type - TyBool
                Pretyped Expr: Unit - TyUnit
                Pretyped Expr: Boolean true - TyBool
        PatternExpr - TyVar t9
            Pretyped MatchedExpr - TyVar t8: Option Some
                Pretyped MatchedExpr - TyVar t9: Var y
            Pretyped PatternBlockExpr Block Expr Type - TyVar t9
                Pretyped Expr: Var y - TyVar t9 |}];
      let constructors_env =
        [
          Type_defns_env.ConstructorEnvEntry
            ( Type_name.of_string "custom_type",
              Constructor_name.of_string "C1",
              [ TEInt mock_loc ] );
        ]
      in
      let typing_context : Type_infer_types.typing_context =
        [
          Type_context_env.TypingContextEntry
            ( Var_name.of_string "x",
              Type_infer_types.TyCustom (Type_name.of_string "custom_type") );
        ]
      in
      let match_expr =
        Match
          ( mock_loc,
            Var_name.of_string "x",
            [
              Parsing.Parser_ast.MPattern
                ( mock_loc,
                  Parsing.Parser_ast.MUnderscore mock_loc,
                  Block (mock_loc, [ Unit mock_loc; Integer (mock_loc, 0) ]) );
              Parsing.Parser_ast.MPattern
                ( mock_loc,
                  Parsing.Parser_ast.MVariable (mock_loc, Var_name.of_string "x"),
                  Block (mock_loc, [ Unit mock_loc; Integer (mock_loc, 1) ]) );
              Parsing.Parser_ast.MPattern
                ( mock_loc,
                  Parsing.Parser_ast.MConstructor
                    ( mock_loc,
                      Constructor_name.of_string "C1",
                      [
                        Parsing.Parser_ast.MVariable
                          (mock_loc, Var_name.of_string "x");
                      ] ),
                  Block
                    ( mock_loc,
                      [
                        Unit mock_loc;
                        Variable (mock_loc, Var_name.of_string "x");
                      ] ) );
            ] )
      in
      match
        Type_infer_constraints_generator.generate_constraints constructors_env
          [] typing_context match_expr ~verbose:true
      with
      | Error _ -> ()
      | Ok (_, _, _, pretyped_expr) ->
          Pprint_pretyped_ast.pprint_pretyped_expr Fmt.stdout ~indent:""
            pretyped_expr;
          [%expect
            {|
    Actual expr:
    Expr: Unit

    => Typing Context:
    x : TyCustom custom_type
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Int: 0

    => Typing Context:
    x : TyCustom custom_type
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Unit
        Expr: Int: 0

    => Typing Context:
    x : TyCustom custom_type
    => Block Expr Ty:
    TyInt
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: Unit

    => Typing Context:
    x : TyVar t13
    x : TyCustom custom_type
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Int: 1

    => Typing Context:
    x : TyVar t13
    x : TyCustom custom_type
    => Expr Ty:
    TyInt
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Unit
        Expr: Int: 1

    => Typing Context:
    x : TyVar t13
    x : TyCustom custom_type
    => Block Expr Ty:
    TyInt
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: Unit

    => Typing Context:
    x : TyVar t14
    x : TyCustom custom_type
    => Expr Ty:
    TyUnit
    => Expr Constraints:
    -------------------------

    Actual expr:
    Expr: Var: x

    => Typing Context:
    x : TyVar t14
    x : TyCustom custom_type
    => Expr Ty:
    TyVar t14
    => Expr Constraints:
    -------------------------

    Block expr:
     Block
        Expr: Unit
        Expr: Var: x

    => Typing Context:
    x : TyVar t14
    x : TyCustom custom_type
    => Block Expr Ty:
    TyVar t14
    => Block Expr Constraints:
    (TyUnit, TyUnit)
    -------------------------

    Actual expr:
    Expr: Match
        Match Var: x
        PatternExpr
            MatchedExpr: Underscore
            PatternBlockExpr Block
                Expr: Unit
                Expr: Int: 0
        PatternExpr
            MatchedExpr: Var - x
            PatternBlockExpr Block
                Expr: Unit
                Expr: Int: 1
        PatternExpr
            MatchedExpr: Constructor - C1
                MatchedExpr: Var - x
            PatternBlockExpr Block
                Expr: Unit
                Expr: Var: x

    => Typing Context:
    x : TyCustom custom_type
    => Expr Ty:
    TyVar t11
    => Expr Constraints:
    (TyCustom custom_type, TyCustom custom_type)
    (TyVar t11, TyVar t14)
    (TyVar t14, TyInt)
    (TyUnit, TyUnit)
    (TyCustom custom_type, TyVar t13)
    (TyVar t11, TyInt)
    (TyUnit, TyUnit)
    (TyCustom custom_type, TyVar t12)
    (TyVar t11, TyInt)
    (TyUnit, TyUnit)
    -------------------------

    Pretyped Expr: Match - TyVar t11
        Match Var: x
        PatternExpr - TyVar t14
            Pretyped MatchedExpr - TyCustom custom_type: C1
                Pretyped MatchedExpr - TyVar t14: Var x
            Pretyped PatternBlockExpr Block Expr Type - TyVar t14
                Pretyped Expr: Unit - TyUnit
                Pretyped Expr: Var x - TyVar t14
        PatternExpr - TyInt
            Pretyped MatchedExpr - TyVar t13: Var x
            Pretyped PatternBlockExpr Block Expr Type - TyInt
                Pretyped Expr: Unit - TyUnit
                Pretyped Expr: Integer 1 - TyInt
        PatternExpr - TyInt
            Pretyped MatchedExpr - TyVar t12: Underscore
            Pretyped PatternBlockExpr Block Expr Type - TyInt
                Pretyped Expr: Unit - TyUnit
                Pretyped Expr: Integer 0 - TyInt |}]
      )
