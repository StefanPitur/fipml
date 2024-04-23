open Ast.Ast_types
open Core
open Typing.Pprint_type_infer
open Typing.Type_context_env
open Typing.Type_infer
open Typing.Type_infer_types

let%expect_test "Compute free type variables for generalise" =
  let ty_attr_exprs =
    [
      (TyUnit, TyShared);
      (TyVar "t1", TyUnique);
      ( TyPoly
          ( [ "t2" ],
            TyCustom
              ( [ TyVar "t2"; TyVar "t3"; TyVar "t4" ],
                [ TyVarUnique "u1" ],
                [
                  (TyVar "t1", TyVarUnique "u2"); (TyVar "t2", TyVarUnique "u2");
                ],
                Type_name.of_string "custom_type" ) ),
        TyVarUnique "u3" );
    ]
  in
  List.iter ty_attr_exprs ~f:(fun (ty, _) ->
      let free_tys = free_type_vars ty in
      Fmt.pf Fmt.stdout "{%s}@."
        (String.concat ~sep:", " (Set.elements free_tys)));
  [%expect {|
    {}
    {t1}
    {t1, t3, t4} |}]

let%expect_test "Compute bounded type variables from the typing context" =
  let typing_context =
    [
      TypingContextEntry
        (Var_name.of_string "x1", (TyVar "t1", TyVarUnique "u1"));
      TypingContextEntry (Var_name.of_string "x2", (TyInt, TyVarUnique "u2"));
      TypingContextEntry
        ( Var_name.of_string "x3",
          ( TyPoly
              ( [ "t2" ],
                TyCustom
                  ( [ TyVar "t2"; TyVar "t3" ],
                    [ TyVarUnique "u1" ],
                    [ (TyVar "t4", TyVarUnique "u3") ],
                    Type_name.of_string "custom_type" ) ),
            TyShared ) );
    ]
  in
  Fmt.pf Fmt.stdout "{%s}@. "
    (String.concat ~sep:", " (Set.elements (bounded_type_vars typing_context)));
  [%expect {| {t1, t3, t4} |}]

let%expect_test "Generalise" =
  let typing_context =
    [
      TypingContextEntry
        (Var_name.of_string "x1", (TyVar "t1", TyVarUnique "u1"));
    ]
  in
  let ty_attr =
    ( TyCustom
        ( [ TyVar "t1"; TyVar "t2"; TyInt ],
          [ TyVarUnique "u2" ],
          [ (TyVar "t3", TyVarUnique "u1") ],
          Type_name.of_string "custom_type" ),
      TyVarUnique "u3" )
  in
  (match generalise typing_context (Var_name.of_string "x") ty_attr with
  | Ok typing_context -> pprint_typing_context Fmt.stdout typing_context
  | Error err -> Fmt.pf Fmt.stdout "%s@." (Error.to_string_hum err));
  [%expect
    {|
      x : TyAttr - TyPoly - for all (t2, t3). TyCustom (TyVar t1, TyVar t2, TyInt ; TyVarUnique u2 ; TyAttr - TyVar t3 <> TyVarUnique u1) custom_type <> TyVarUnique u3
      x1 : TyAttr - TyVar t1 <> TyVarUnique u1 |}]

let%expect_test "Instantiate" =
  let typing_context =
    [
      TypingContextEntry
        ( Var_name.of_string "x1",
          ( TyPoly
              ( [ "_t1"; "_t2" ],
                TyCustom
                  ( [ TyVar "_t2"; TyVar "_t3"; TyVar "_t1"; TyInt ],
                    [ TyVarUnique "_u1" ],
                    [
                      (TyVar "_t1", TyVarUnique "_u1"); (TyVar "_t3", TyUnique);
                    ],
                    Type_name.of_string "custom_type" ) ),
            TyVarUnique "_u2" ) );
    ]
  in
  (match instantiate (Var_name.of_string "x1") typing_context with
  | Ok ty_attr -> pprint_ty_attr Fmt.stdout ty_attr
  | Error err -> Fmt.pf Fmt.stdout "%s@." (Error.to_string_hum err));
  [%expect
    {| TyAttr - TyCustom (TyVar t2, TyVar _t3, TyVar t1, TyInt ; TyVarUnique _u1 ; TyAttr - TyVar t1 <> TyVarUnique _u1, TyAttr - TyVar _t3 <> TyUnique) custom_type <> TyVarUnique _u2 |}]
