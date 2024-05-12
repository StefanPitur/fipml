open Ast.Ast_types
open Core
open Typing.Pprint_type_infer
open Typing.Type_infer
open Typing.Type_infer_types

let%expect_test "Occurs testing" =
  print_string
    (string_of_bool
       (occurs "t1"
          (TyArrow ((TyVar "t1", fresh_unique ()), (TyInt, fresh_unique ())))));
  [%expect {| true |}]

let%expect_test "Subst testing" =
  let substs : subst list = [ ("t1", TyInt) ] in
  pprint_ty_attr Fmt.stdout
    (ty_attr_subst substs []
       ( TyArrow ((TyVar "t1", fresh_unique ()), (TyVar "t2", fresh_unique ())),
         fresh_unique () ));
  [%expect
    {| TyAttr - TyArrow (TyAttr - TyInt <> TyVarUnique u5 -> TyAttr - TyVar t2 <> TyVarUnique u4) <> TyVarUnique u3 |}]

let%expect_test "small unification" =
  let constraints : constr list =
    [
      ( TyCustom
          ([], [ TyVarUnique "u61" ], [], Type_name.of_string "custom_type"),
        TyVar "t25" );
      ( TyVar "t21",
        TyTuple [ (TyInt, TyVarUnique "u79"); (TyBool, TyVarUnique "u78") ] );
      ( TyCustom
          ([], [ TyVarUnique "u61" ], [], Type_name.of_string "custom_type"),
        TyCustom
          ([], [ TyVarUnique "u72" ], [], Type_name.of_string "custom_type") );
      ( TyVar "t21",
        TyTuple
          [ (TyVar "t24", TyVarUnique "u73"); (TyBool, TyVarUnique "u75") ] );
      (TyVar "t24", TyInt);
      ( TyCustom
          ([], [ TyVarUnique "u61" ], [], Type_name.of_string "custom_type"),
        TyCustom
          ([], [ TyVarUnique "u64" ], [], Type_name.of_string "custom_type") );
      ( TyVar "t21",
        TyTuple [ (TyInt, TyVarUnique "u70"); (TyVar "t23", TyVarUnique "u68") ]
      );
      (TyVar "t23", TyBool);
      ( TyCustom
          ([], [ TyVarUnique "u66" ], [], Type_name.of_string "custom_type"),
        TyCustom ([], [ TyUnique ], [], Type_name.of_string "custom_type") );
      (TyVar "t22", TyInt);
    ]
  in
  let unique_constraints : constr_unique list =
    [
      (TyVarUnique "u60", TyVarUnique "u76");
      (TyVarUnique "u62", TyVarUnique "u77");
      (TyVarUnique "u60", TyVarUnique "u71");
      (TyVarUnique "u62", TyVarUnique "u74");
      (TyVarUnique "u73", TyUnique);
      (TyVarUnique "u60", TyVarUnique "u63");
      (TyVarUnique "u62", TyVarUnique "u69");
      (TyVarUnique "u68", TyVarUnique "u64");
      (TyVarUnique "u65", TyShared);
      (TyVarUnique "u67", TyUnique);
    ]
  in
  match unify constraints with
  | Error err -> print_string (Error.to_string_hum err)
  | Ok (substs, extra_unique_constraints) -> (
      match unify_unique (unique_constraints @ extra_unique_constraints) with
      | Error err -> print_string (Error.to_string_hum err)
      | Ok substs_unique ->
          pprint_substs_unique Fmt.stdout substs_unique;
          [%expect
            {|
          Unique Variable - u75, Unique - TyVarUnique u72
          Unique Variable - u70, Unique - TyUnique
          Unique Variable - u64, Unique - TyVarUnique u72
          Unique Variable - u61, Unique - TyVarUnique u72
          Unique Variable - u79, Unique - TyUnique
          Unique Variable - u78, Unique - TyVarUnique u72
          Unique Variable - u66, Unique - TyUnique
          Unique Variable - u67, Unique - TyUnique
          Unique Variable - u65, Unique - TyShared
          Unique Variable - u68, Unique - TyVarUnique u72
          Unique Variable - u74, Unique - TyVarUnique u69
          Unique Variable - u71, Unique - TyVarUnique u63
          Unique Variable - u73, Unique - TyUnique
          Unique Variable - u77, Unique - TyVarUnique u69
          Unique Variable - u76, Unique - TyVarUnique u63
          Unique Variable - u62, Unique - TyVarUnique u69
          Unique Variable - u60, Unique - TyVarUnique u63 |}];
          let substs = apply_substs_unique_to_substs substs_unique substs in
          pprint_substs Fmt.stdout substs;
          [%expect
            {|
          Type Variable - t22, Type - TyInt
          Type Variable - t23, Type - TyBool
          Type Variable - t24, Type - TyInt
          Type Variable - t21, Type - TyTuple (TyAttr - TyInt <> TyUnique, TyAttr - TyBool <> TyVarUnique u72)
          Type Variable - t25, Type - TyCustom ( ; TyVarUnique u72 ; ) custom_type |}]
      )
