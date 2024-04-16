open Ast.Ast_types
open Core
open Typing.Pprint_type_infer
open Typing.Type_context_env
open Typing.Type_infer
open Typing.Type_infer_types

let%expect_test "Compute free type variables for generalise" =
  let ty_exprs =
    [
      TyUnit;
      TyVar "t1";
      TyPoly
        ( [ "t2" ],
          TyCustom
            ( [ TyVar "t2"; TyVar "t3"; TyVar "t4" ],
              Type_name.of_string "custom_type" ) );
    ]
  in
  List.iter ty_exprs ~f:(fun ty ->
      let free_tys = free_type_vars ty in
      Fmt.pf Fmt.stdout "{%s}@."
        (String.concat ~sep:", " (Set.elements free_tys)));
  [%expect {|
    {}
    {t1}
    {t3, t4} |}]

let%expect_test "Compute bounded type variables from the typing context" =
  let typing_context =
    [
      TypingContextEntry (Var_name.of_string "x1", TyVar "t1");
      TypingContextEntry (Var_name.of_string "x2", TyInt);
      TypingContextEntry
        ( Var_name.of_string "x3",
          TyPoly
            ( [ "t2" ],
              TyCustom
                ([ TyVar "t2"; TyVar "t3" ], Type_name.of_string "custom_type")
            ) );
    ]
  in
  Fmt.pf Fmt.stdout "{%s}@. "
    (String.concat ~sep:", " (Set.elements (bounded_type_vars typing_context)));
  [%expect {| {t1, t3} |}]

let%expect_test "Generalise" =
  let typing_context =
    [ TypingContextEntry (Var_name.of_string "x1", TyVar "t1") ]
  in
  let ty =
    TyCustom
      ([ TyVar "t1"; TyVar "t2"; TyInt ], Type_name.of_string "custom_type")
  in
  (match generalise typing_context (Var_name.of_string "x") ty with
  | Ok typing_context -> pprint_typing_context Fmt.stdout typing_context
  | Error err -> Fmt.pf Fmt.stdout "%s@." (Error.to_string_hum err));
  [%expect
    {|
    x : TyPoly - for all (t2). TyCustom (TyVar t1, TyVar t2, TyInt) custom_type
    x1 : TyVar t1 |}]

let%expect_test "Instantiate" =
  let typing_context =
    [
      TypingContextEntry
        ( Var_name.of_string "x1",
          TyPoly
            ( [ "_t1"; "_t2" ],
              TyCustom
                ( [ TyVar "_t2"; TyVar "_t3"; TyVar "_t1"; TyInt ],
                  Type_name.of_string "custom_type" ) ) );
    ]
  in
  (match instantiate (Var_name.of_string "x1") typing_context with
  | Ok ty -> pprint_ty Fmt.stdout ty
  | Error err -> Fmt.pf Fmt.stdout "%s@." (Error.to_string_hum err));
  [%expect {| TyCustom (TyVar t2, TyVar _t3, TyVar t1, TyInt) custom_type |}]
