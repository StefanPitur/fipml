open Ast.Ast_types
open Core
open Typing.Pprint_type_infer
open Typing.Type_infer
open Typing.Type_infer_types

let%expect_test "Occurs testing" =
  print_string (string_of_bool (occurs "t1" (TyArrow (TyVar "t1", TyInt))));
  [%expect {| true |}]

let%expect_test "Subst testing" =
  let substs : subst list = [ ("t1", TyInt) ] in
  pprint_ty Fmt.stdout (ty_subst substs (TyArrow (TyVar "t1", TyVar "t2")));
  [%expect {| TyArrow (TyInt -> TyVar t2) |}]

let%expect_test "small unification" =
  let constraints : constr list =
    [
      ( TyCustom ([], Type_name.of_string "custom_type"),
        TyCustom ([], Type_name.of_string "custom_type") );
      (TyVar "t12", TyVar "t15");
      (TyVar "t15", TyInt);
      (TyUnit, TyUnit);
      (TyCustom ([], Type_name.of_string "custom_type"), TyVar "t14");
      (TyVar "t12", TyInt);
      (TyUnit, TyUnit);
      (TyCustom ([], Type_name.of_string "custom_type"), TyVar "t13");
      (TyVar "t12", TyInt);
      (TyUnit, TyUnit);
    ]
  in
  match unify constraints with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok substs ->
      pprint_substs Fmt.stdout substs;
      [%expect
        {|
    Type Variable - t13, Type - TyCustom () custom_type
    Type Variable - t14, Type - TyCustom () custom_type
    Type Variable - t15, Type - TyInt
    Type Variable - t12, Type - TyInt |}]
