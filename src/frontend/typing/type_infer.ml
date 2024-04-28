open Ast.Ast_types
open Core
open Functions_env
open Parsing
open Type_context_env
open Type_defns_env
open Type_infer_types

exception PartialFunctionApplicationNotAllowed
exception TyNotMatching
exception UnableToUnify of string
exception PolymorphicExpected of string

module FreeSet = Set.Make (String)

let unify (constraints : constr list) :
    (subst list * constr_unique list) Or_error.t =
  let rec unify (constraints : constr list)
      (extra_unique_constraints : constr_unique list) (substs : subst list) =
    match constraints with
    | [] -> Ok (substs, extra_unique_constraints)
    | (t1, t2) :: constraints when ty_equal t1 t2 ->
        unify constraints extra_unique_constraints substs
    | (TyVar type_var, t) :: constraints when not (occurs type_var t) ->
        let ts = ty_subst [ (type_var, t) ] [] in
        unify
          (List.map constraints ~f:(fun (ty1, ty2) -> (ts ty1, ts ty2)))
          extra_unique_constraints
          ((type_var, t) :: List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2)))
    | (t, TyVar type_var) :: constraints when not (occurs type_var t) ->
        let ts = ty_subst [ (type_var, t) ] [] in
        unify
          (List.map constraints ~f:(fun (ty1, ty2) -> (ts ty1, ts ty2)))
          extra_unique_constraints
          ((type_var, t) :: List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2)))
    | ( TyArrow ((ty11, ty_unique11), (ty12, ty_unique12)),
        TyArrow ((ty21, ty_unique21), (ty22, ty_unique22)) )
      :: constraints ->
        unify
          ((ty11, ty21) :: (ty12, ty22) :: constraints)
          ((ty_unique11, ty_unique21) :: (ty_unique12, ty_unique22)
         :: extra_unique_constraints)
          substs
    | (TyTuple ty_attrs1, TyTuple ty_attrs2) :: constraints
      when List.length ty_attrs1 = List.length ty_attrs2 ->
        let ty_constraints, ty_unique_constraints =
          List.fold2_exn ty_attrs1 ty_attrs2 ~init:([], [])
            ~f:(fun
                (acc_ty_constraints, acc_ty_unique_constraints)
                (ty1, ty_unique1)
                (ty2, ty_unique2)
              ->
              ( (ty1, ty2) :: acc_ty_constraints,
                (ty_unique1, ty_unique2) :: acc_ty_unique_constraints ))
        in
        unify
          (ty_constraints @ constraints)
          (ty_unique_constraints @ extra_unique_constraints)
          substs
    | ( TyCustom (tys1, ty_uniques1, ty_attrs1, type_name1),
        TyCustom (tys2, ty_uniques2, ty_attrs2, type_name2) )
      :: constraints
      when Type_name.( = ) type_name1 type_name2
           && List.length tys1 = List.length tys2
           && List.length ty_attrs1 = List.length ty_attrs2 ->
        let tys_constraints =
          List.map2_exn tys1 tys2 ~f:(fun ty1 ty2 -> (ty1, ty2))
        in
        let ty_unique_constraints_from_ty_uniques =
          List.map2_exn ty_uniques1 ty_uniques2 ~f:(fun ty_unique1 ty_unique2 ->
              (ty_unique1, ty_unique2))
        in
        let ty_constraints_from_ty_attrs, ty_unique_constraints_from_ty_attrs =
          List.fold2_exn ty_attrs1 ty_attrs2 ~init:([], [])
            ~f:(fun
                (acc_ty_constraints, acc_ty_unique_constraints)
                (ty1, ty_unique1)
                (ty2, ty_unique2)
              ->
              ( (ty1, ty2) :: acc_ty_constraints,
                (ty_unique1, ty_unique2) :: acc_ty_unique_constraints ))
        in
        unify
          (tys_constraints @ ty_constraints_from_ty_attrs @ constraints)
          (ty_unique_constraints_from_ty_uniques
         @ ty_unique_constraints_from_ty_attrs @ extra_unique_constraints)
          substs
    | (ty1, ty2) :: _ ->
        let error_string = Fmt.str "Unable to unify types %s and %s@." in
        let string_ty1 = Pprint_type_infer.string_of_ty ty1 in
        let string_ty2 = Pprint_type_infer.string_of_ty ty2 in
        Or_error.of_exn (UnableToUnify (error_string string_ty1 string_ty2))
  in
  unify constraints [] []

let unify_unique (unique_constraints : constr_unique list) :
    subst_unique list Or_error.t =
  let rec unify_unique (unique_constraints : constr_unique list)
      (substs_unique : subst_unique list) =
    match unique_constraints with
    | [] -> Ok substs_unique
    | (u1, u2) :: unique_constraints when ty_unique_equal u1 u2 ->
        unify_unique unique_constraints substs_unique
    | (TyVarUnique var, u) :: unique_constraints
    | (u, TyVarUnique var) :: unique_constraints ->
        let us = ty_unique_subst [ (var, u) ] in
        unify_unique
          (List.map unique_constraints ~f:(fun (ty_unique1, ty_unique2) ->
               (us ty_unique1, us ty_unique2)))
          ((var, u)
          :: List.map substs_unique ~f:(fun (ty_unique_var, ty_unique) ->
                 (ty_unique_var, us ty_unique)))
    | (u1, u2) :: _ ->
        let error_string =
          Fmt.str "Unable to unify unique attributes %s and %s@."
        in
        let string_u1 = Pprint_type_infer.string_of_ty_unique u1 in
        let string_u2 = Pprint_type_infer.string_of_ty_unique u2 in
        Or_error.of_exn (UnableToUnify (error_string string_u1 string_u2))
  in
  unify_unique unique_constraints []

let unify_substs (substs1 : subst list) (substs2 : subst list) :
    (subst list * constr_unique list) Or_error.t =
  let unified_substs = ref substs2 in
  let unified_constrs = ref [] in
  let unify_substs (ty_var, ty) =
    match
      List.find !unified_substs ~f:(fun (merged_ty_var, _) ->
          String.( = ) merged_ty_var ty_var)
    with
    | None -> unified_substs := (ty_var, ty) :: !unified_substs
    | Some (_, matched_ty) ->
        if ty_equal matched_ty ty then ()
        else unified_constrs := (ty, matched_ty) :: !unified_constrs
  in
  List.iter substs1 ~f:unify_substs;
  List.iter !unified_substs ~f:(fun (ty_var, ty) ->
      unified_constrs := (TyVar ty_var, ty) :: !unified_constrs);
  unify !unified_constrs

let rec free_type_vars (ty : ty) : FreeSet.t =
  match ty with
  | TyInt | TyUnit | TyBool -> FreeSet.empty
  | TyVar ty_var -> FreeSet.singleton ty_var
  | TyPoly (polys, ty) -> Set.diff (free_type_vars ty) (FreeSet.of_list polys)
  | TyCustom (tys, _, ty_attrs, _) ->
      let tys_free_vars = FreeSet.union_list (List.map tys ~f:free_type_vars) in
      let ty_attrs_free_vars =
        FreeSet.union_list
          (List.map ty_attrs ~f:(fun (ty, _) -> free_type_vars ty))
      in
      Set.union tys_free_vars ty_attrs_free_vars
  | TyTuple ty_attrs ->
      FreeSet.union_list
        (List.map ty_attrs ~f:(fun (ty, _) -> free_type_vars ty))
  | TyArrow ((ty1, _), (ty2, _)) ->
      Set.union (free_type_vars ty1) (free_type_vars ty2)

let bounded_type_vars (typing_context : typing_context) : FreeSet.t =
  let free_tys =
    List.map typing_context ~f:(fun (TypingContextEntry (_, (ty, _))) ->
        free_type_vars ty)
  in
  FreeSet.union_list free_tys

let rec generalise (typing_context : typing_context) (var_name : Var_name.t)
    ((var_ty, var_ty_unique) : ty_attr) : typing_context Or_error.t =
  let generalisable_ty_vars =
    Set.diff (free_type_vars var_ty) (bounded_type_vars typing_context)
  in
  let poly_strings = Set.elements generalisable_ty_vars in
  extend_typing_context typing_context var_name
    (TyPoly (poly_strings, var_ty), var_ty_unique)

and instantiate (var_name : Var_name.t) (typing_context : typing_context) :
    ty_attr Or_error.t =
  let open Result in
  get_var_type typing_context var_name >>= fun (var_ty, var_ty_unique) ->
  match var_ty with
  | TyPoly (poly_strings, ty) ->
      let substs =
        List.map poly_strings ~f:(fun poly_string -> (poly_string, fresh ()))
      in
      Ok (ty_subst substs [] ty, var_ty_unique)
  | _ -> Ok (var_ty, var_ty_unique)

and type_infer (types_env : types_env) (constructors_env : constructors_env)
    (functions_env : functions_env) (typing_context : typing_context)
    (initial_unique_constraints : constr_unique list)
    (expected_ty_attr_option : ty_attr option) (expr : Parser_ast.expr)
    ~(verbose : bool) :
    (Typed_ast.expr * subst list * subst_unique list) Or_error.t =
  let open Result in
  generate_constraints types_env constructors_env functions_env typing_context
    expr ~verbose
  >>= fun ( _,
            (ty, ty_unique),
            constraints,
            unique_constraints,
            let_poly_substs,
            pretyped_expr ) ->
  let constraints, unique_constraints =
    match expected_ty_attr_option with
    | None -> (constraints, unique_constraints)
    | Some (expected_ty, expected_ty_unique) ->
        ( (ty, expected_ty) :: constraints,
          (ty_unique, expected_ty_unique) :: unique_constraints )
  in
  unify constraints >>= fun (constraints_substs, extra_unique_constraints) ->
  unify_substs constraints_substs let_poly_substs
  >>= fun (substs, let_polys_extra_unique_constraints) ->
  let unique_constraints =
    unique_constraints @ extra_unique_constraints
    @ let_polys_extra_unique_constraints @ initial_unique_constraints
  in
  unify_unique unique_constraints >>= fun substs_unique ->
  let substs = apply_substs_unique_to_substs substs_unique substs in
  Ok
    ( Or_error.ok_exn
        (Construct_typed_ast.construct_typed_ast_expr pretyped_expr substs
           substs_unique),
      substs,
      substs_unique )

and generate_constraints (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (expr : Parser_ast.expr) ~(verbose : bool)
    :
    (Type_infer_types.typing_context
    * Type_infer_types.ty_attr
    * Type_infer_types.constr list
    * Type_infer_types.constr_unique list
    * subst list
    * Pretyped_ast.expr)
    Or_error.t =
  let open Result in
  (match expr with
  | UnboxedSingleton (loc, value) ->
      generate_constraints_value_expr types_env constructors_env functions_env
        typing_context value ~verbose
      >>= fun ( value_ty_attr,
                value_constraints,
                value_unique_constraints,
                pretyped_value ) ->
      Ok
        ( typing_context,
          value_ty_attr,
          value_constraints,
          value_unique_constraints,
          [],
          Pretyped_ast.UnboxedSingleton (loc, value_ty_attr, pretyped_value) )
  | UnboxedTuple (loc, values) ->
      let u = fresh_unique () in
      let ( values_types,
            values_constraints,
            value_unique_constraints,
            values_pretyped ) =
        List.fold_right values ~init:([], [], [], [])
          ~f:(fun
              value
              ( acc_types,
                acc_constraints,
                acc_unique_constraints,
                acc_pretyped_values )
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context value ~verbose
              >>= fun ( value_ty,
                        value_constraints,
                        value_unique_constraints,
                        pretyped_value ) ->
                Ok
                  ( value_ty :: acc_types,
                    value_constraints @ acc_constraints,
                    value_unique_constraints @ acc_unique_constraints,
                    pretyped_value :: acc_pretyped_values ) ))
      in
      Ok
        ( typing_context,
          (TyTuple values_types, u),
          values_constraints,
          value_unique_constraints,
          [],
          Pretyped_ast.UnboxedTuple
            (loc, (TyTuple values_types, u), values_pretyped) )
  | Let (loc, var_names, vars_expr, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context vars_expr ~verbose
      >>= fun ( _,
                var_ty_attr,
                var_constrs,
                var_unique_constrs,
                var_pretyped_substs,
                pretyped_vars ) ->
      unify var_constrs >>= fun (var_substs, extra_var_unique_constrs) ->
      let ((sub_var_ty, sub_var_ty_unique) as sub_var_ty_attr) =
        ty_attr_subst var_substs [] var_ty_attr
      in
      let sub_typing_context = ty_subst_context typing_context var_substs [] in
      let sharing_analysis_map = get_sharing_analysis expr in
      let ( extended_typing_context,
            sharing_analysis_unique_constraints,
            vars_ty_attrs ) =
        Or_error.ok_exn
          (match sub_var_ty with
          | TyTuple ty_attrs ->
              if List.length var_names = 1 then Or_error.of_exn TyNotMatching
              else
                Ok
                  (List.fold_right2_exn var_names ty_attrs
                     ~init:(sub_typing_context, [], [])
                     ~f:(fun
                         var_name
                         ((var_ty, var_ty_unique) as var_ty_attr)
                         ( typing_context,
                           acc_sharing_unique_constraints,
                           acc_var_ty_attrs )
                       ->
                       let ty_unique_sharing =
                         get_ty_unique_from_sharing_analysis
                           sharing_analysis_map var_name
                       in
                       ( Or_error.ok_exn
                           (generalise typing_context var_name
                              (var_ty, var_ty_unique)),
                         (ty_unique_sharing, var_ty_unique)
                         :: acc_sharing_unique_constraints,
                         var_ty_attr :: acc_var_ty_attrs )))
          | _ -> (
              match var_names with
              | [ var_name ] ->
                  let ty_unique_sharing =
                    get_ty_unique_from_sharing_analysis sharing_analysis_map
                      var_name
                  in
                  Ok
                    ( Or_error.ok_exn
                        (generalise sub_typing_context var_name
                           (sub_var_ty, sub_var_ty_unique)),
                      [ (ty_unique_sharing, sub_var_ty_unique) ],
                      [ sub_var_ty_attr ] )
              | _ -> Or_error.of_exn TyNotMatching))
      in
      generate_constraints types_env constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun ( _,
                expr_ty_attr,
                expr_constrs,
                expr_unique_constrs,
                expr_substs,
                pretyped_expr ) ->
      unify_substs expr_substs var_substs
      >>= fun (expr_substs, extra_unique_constrs1) ->
      unify_substs expr_substs var_pretyped_substs
      >>= fun (expr_substs, extra_unique_constrs2) ->
      Ok
        ( extended_typing_context,
          expr_ty_attr,
          expr_constrs,
          sharing_analysis_unique_constraints @ extra_unique_constrs1
          @ extra_unique_constrs2 @ extra_var_unique_constrs
          @ expr_unique_constrs @ var_unique_constrs,
          expr_substs,
          Pretyped_ast.Let
            ( loc,
              expr_ty_attr,
              vars_ty_attrs,
              var_names,
              pretyped_vars,
              pretyped_expr ) )
  | FunApp (loc, app_var_name, app_values) ->
      get_var_type typing_context app_var_name >>= fun app_var_ty_attr ->
      get_ty_attr_function_signature app_var_ty_attr
      >>= fun (app_params_ty_attrs, app_return_ty_attr) ->
      let ( app_params_constraints,
            app_params_unique_constraints,
            app_params_pretyped ) =
        List.fold_right2_exn app_params_ty_attrs app_values ~init:([], [], [])
          ~f:(fun
              (app_param_ty, app_param_ty_unique)
              app_value
              ( acc_params_constraints,
                acc_params_unique_constraints,
                acc_params_pretyped )
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context app_value ~verbose
              >>= fun ( (value_ty, value_ty_unique),
                        value_constraints,
                        value_unique_constraints,
                        pretyped_value ) ->
                Ok
                  ( ((value_ty, app_param_ty) :: value_constraints)
                    @ acc_params_constraints,
                    (value_ty_unique, app_param_ty_unique)
                    :: value_unique_constraints
                    @ acc_params_unique_constraints,
                    pretyped_value :: acc_params_pretyped ) ))
      in
      Ok
        ( typing_context,
          app_return_ty_attr,
          app_params_constraints,
          app_params_unique_constraints,
          [],
          Pretyped_ast.FunApp
            (loc, app_return_ty_attr, app_var_name, app_params_pretyped) )
  | FunCall (loc, function_name, values) ->
      Functions_env.get_function_by_name loc function_name functions_env
      >>= fun (FunctionEnvEntry
                (_, _, _, function_args_types, _, function_return_type)) ->
      if List.length values <> List.length function_args_types then
        Or_error.of_exn PartialFunctionApplicationNotAllowed
      else
        let ( ty_scheme_assoc_list,
              ty_unique_scheme_assoc_list,
              ty_attr_scheme_assoc_list ) =
          get_type_expr_scheme_assoc_lists
            (function_args_types @ [ function_return_type ])
        in
        let args_constraints, args_unique_constraints, args_pretyped =
          List.fold_right2_exn function_args_types values ~init:([], [], [])
            ~f:(fun
                function_arg_type
                value
                (acc_constraints, acc_unique_constraints, acc_pretyped_values)
              ->
              Or_error.ok_exn
                ( generate_constraints_value_expr types_env constructors_env
                    functions_env typing_context value ~verbose
                >>= fun ( (value_ty, value_ty_unique),
                          value_constraints,
                          value_unique_constraints,
                          pretyped_value ) ->
                  let function_arg_ty, function_arg_ty_unique =
                    convert_ast_type_to_ty_attr function_arg_type
                      ty_scheme_assoc_list ty_unique_scheme_assoc_list
                      ty_attr_scheme_assoc_list
                  in
                  Ok
                    ( ((function_arg_ty, value_ty) :: value_constraints)
                      @ acc_constraints,
                      (function_arg_ty_unique, value_ty_unique)
                      :: value_unique_constraints
                      @ acc_unique_constraints,
                      pretyped_value :: acc_pretyped_values ) ))
        in
        let expr_type =
          convert_ast_type_to_ty_attr function_return_type ty_scheme_assoc_list
            ty_unique_scheme_assoc_list ty_attr_scheme_assoc_list
        in
        Ok
          ( typing_context,
            expr_type,
            args_constraints,
            args_unique_constraints,
            [],
            Pretyped_ast.FunCall (loc, expr_type, function_name, args_pretyped)
          )
  | If (loc, expr_cond, expr_then) ->
      let t = fresh () in
      let u = fresh_unique () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr_cond ~verbose
      >>= fun ( _,
                (expr_cond_ty, _),
                expr_cond_constrs,
                expr_cond_unique_constrs,
                expr_cond_substs,
                pretyped_expr ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun ( _,
                (expr_then_ty, expr_then_ty_unique),
                expr_then_constrs,
                expr_then_unique_constrs,
                expr_then_substs,
                pretyped_then_expr ) ->
      unify_substs expr_cond_substs expr_then_substs
      >>= fun (expr_substs, extra_unique_constrs) ->
      Ok
        ( typing_context,
          (t, u),
          [ (expr_cond_ty, TyBool); (t, expr_then_ty) ]
          @ expr_cond_constrs @ expr_then_constrs,
          ((u, expr_then_ty_unique) :: extra_unique_constrs)
          @ expr_cond_unique_constrs @ expr_then_unique_constrs,
          expr_substs,
          Pretyped_ast.If (loc, (t, u), pretyped_expr, pretyped_then_expr) )
  | IfElse (loc, expr_cond, expr_then, expr_else) ->
      let t = fresh () in
      let u = fresh_unique () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr_cond ~verbose
      >>= fun ( _,
                (expr_cond_ty, _),
                expr_cond_constrs,
                expr_cond_unique_constrs,
                expr_cond_substs,
                pretyped_expr ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun ( _,
                (expr_then_ty, expr_then_ty_unique),
                expr_then_constrs,
                expr_then_unique_constrs,
                expr_then_substs,
                pretyped_expr_then ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_else ~verbose
      >>= fun ( _,
                (expr_else_ty, expr_else_ty_unique),
                expr_else_constrs,
                expr_else_unique_constrs,
                expr_else_substs,
                pretyped_expr_else ) ->
      unify_substs expr_cond_substs expr_then_substs
      >>= fun (expr_substs, extra_unique_constraints1) ->
      unify_substs expr_substs expr_else_substs
      >>= fun (expr_substs, extra_unique_constraints2) ->
      Ok
        ( typing_context,
          (t, u),
          [ (expr_cond_ty, TyBool); (t, expr_then_ty); (t, expr_else_ty) ]
          @ expr_cond_constrs @ expr_then_constrs @ expr_else_constrs,
          [ (u, expr_then_ty_unique); (u, expr_else_ty_unique) ]
          @ expr_cond_unique_constrs @ expr_then_unique_constrs
          @ expr_else_unique_constrs @ extra_unique_constraints1
          @ extra_unique_constraints2,
          expr_substs,
          Pretyped_ast.IfElse
            (loc, (t, u), pretyped_expr, pretyped_expr_then, pretyped_expr_else)
        )
  | Match (loc, var_name, pattern_exprs) ->
      let t = fresh () in
      let u = fresh_unique () in
      instantiate var_name typing_context
      >>= fun (matched_var_ty, matched_var_ty_unique) ->
      Ok
        (List.fold_right pattern_exprs ~init:([], [], [], [])
           ~f:(fun
               (MPattern (loc, matched_expr, pattern_expr))
               ( acc_constraints,
                 acc_unique_constraints,
                 acc_substs,
                 acc_pretyped_pattern_exprs )
             ->
             let get_ty_unique_from_sharing_analysis =
               get_ty_unique_from_sharing_analysis
                 (get_sharing_analysis pattern_expr)
             in
             Or_error.ok_exn
               ( generate_constraints_matched_expr types_env constructors_env
                   matched_expr get_ty_unique_from_sharing_analysis ~verbose
               >>= fun ( matched_typing_context,
                         (matched_expr_ty, matched_expr_ty_unique),
                         match_expr_constraints,
                         match_expr_unique_constraints,
                         pretyped_matched_expr ) ->
                 let combined_typing_contexts =
                   Or_error.ok_exn
                     (combine_typing_contexts matched_typing_context
                        typing_context)
                 in
                 generate_constraints types_env constructors_env functions_env
                   combined_typing_contexts pattern_expr ~verbose
                 >>= fun ( _,
                           (pattern_expr_ty, pattern_expr_ty_unique),
                           pattern_expr_constraints,
                           pattern_expr_unique_constraints,
                           pattern_expr_substs,
                           pretyped_pattern_expr ) ->
                 unify_substs pattern_expr_substs acc_substs
                 >>= fun (acc_substs, extra_unique_constraints) ->
                 Ok
                   ( (matched_var_ty, matched_expr_ty)
                     :: (t, pattern_expr_ty) :: match_expr_constraints
                     @ pattern_expr_constraints @ acc_constraints,
                     (matched_var_ty_unique, matched_expr_ty_unique)
                     :: (u, pattern_expr_ty_unique)
                     :: pattern_expr_unique_constraints
                     @ match_expr_unique_constraints @ extra_unique_constraints
                     @ acc_unique_constraints,
                     acc_substs,
                     Pretyped_ast.MPattern
                       ( loc,
                         (pattern_expr_ty, pattern_expr_ty_unique),
                         pretyped_matched_expr,
                         pretyped_pattern_expr )
                     :: acc_pretyped_pattern_exprs ) )))
      >>= fun ( match_expr_constraints,
                match_expr_unique_constraints,
                match_expr_substs,
                pretyped_pattern_exprs ) ->
      Ok
        ( typing_context,
          (t, u),
          match_expr_constraints,
          match_expr_unique_constraints,
          match_expr_substs,
          Pretyped_ast.Match
            ( loc,
              (t, u),
              (matched_var_ty, matched_var_ty_unique),
              var_name,
              pretyped_pattern_exprs ) )
  | UnOp (loc, unary_op, expr) -> (
      let u = fresh_unique () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun ( _,
                (expr_ty, _),
                expr_constrs,
                expr_unique_constrs,
                expr_substs,
                pretyped_expr ) ->
      match unary_op with
      | UnOpNeg ->
          Ok
            ( typing_context,
              (expr_ty, u),
              (expr_ty, TyInt) :: expr_constrs,
              expr_unique_constrs,
              expr_substs,
              Pretyped_ast.UnOp (loc, (expr_ty, u), UnOpNeg, pretyped_expr) )
      | UnOpNot ->
          Ok
            ( typing_context,
              (expr_ty, u),
              (expr_ty, TyBool) :: expr_constrs,
              expr_unique_constrs,
              expr_substs,
              Pretyped_ast.UnOp (loc, (expr_ty, u), UnOpNot, pretyped_expr) ))
  | BinaryOp (loc, binary_op, expr1, expr2) -> (
      let u = fresh_unique () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr1 ~verbose
      >>= fun ( _,
                (expr1_ty, _),
                expr1_constrs,
                expr1_unique_constrs,
                expr1_substs,
                pretyped_expr1 ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr2 ~verbose
      >>= fun ( _,
                (expr2_ty, _),
                expr2_constrs,
                expr2_unique_constrs,
                expr2_substs,
                pretyped_expr2 ) ->
      unify_substs expr1_substs expr2_substs
      >>= fun (expr_substs, extra_unique_constraints) ->
      match binary_op with
      | (BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpMod) as binary_op
        ->
          Ok
            ( typing_context,
              (TyInt, u),
              ((expr1_ty, TyInt) :: (expr2_ty, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              extra_unique_constraints @ expr1_unique_constrs
              @ expr2_unique_constrs,
              expr_substs,
              Pretyped_ast.BinaryOp
                (loc, (TyInt, u), binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpLt | BinOpGt | BinOpLeq | BinOpGeq) as binary_op ->
          Ok
            ( typing_context,
              (TyBool, u),
              ((expr1_ty, TyInt) :: (expr2_ty, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              extra_unique_constraints @ expr1_unique_constrs
              @ expr2_unique_constrs,
              expr_substs,
              Pretyped_ast.BinaryOp
                (loc, (TyBool, u), binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpEq | BinOpNeq) as binary_op ->
          Ok
            ( typing_context,
              (TyBool, u),
              ((expr1_ty, expr2_ty) :: expr1_constrs) @ expr2_constrs,
              extra_unique_constraints @ expr1_unique_constrs
              @ expr2_unique_constrs,
              expr_substs,
              Pretyped_ast.BinaryOp
                (loc, (TyBool, u), binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpAnd | BinOpOr) as binary_op ->
          Ok
            ( typing_context,
              (TyBool, u),
              ((expr1_ty, TyBool) :: (expr2_ty, TyBool) :: expr1_constrs)
              @ expr2_constrs,
              extra_unique_constraints @ expr1_unique_constrs
              @ expr2_unique_constrs,
              expr_substs,
              Pretyped_ast.BinaryOp
                (loc, (TyBool, u), binary_op, pretyped_expr1, pretyped_expr2) ))
  | Drop (loc, var_name, expr) ->
      get_var_type typing_context var_name >>= fun (var_ty, var_ty_unique) ->
      remove_var_from_typing_context typing_context var_name
      >>= fun extended_typing_context ->
      generate_constraints types_env constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun ( _,
                expr_ty_attr,
                expr_constraints,
                expr_unique_constraints,
                expr_substs,
                pretyped_expr ) ->
      Ok
        ( typing_context,
          expr_ty_attr,
          expr_constraints,
          (var_ty_unique, TyUnique) :: expr_unique_constraints,
          expr_substs,
          Pretyped_ast.Drop
            (loc, expr_ty_attr, (var_ty, var_ty_unique), var_name, pretyped_expr)
        )
  | Free (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun ( _,
                expr_ty_attr,
                expr_constraints,
                expr_unique_constraints,
                expr_substs,
                pretyped_expr ) ->
      Ok
        ( typing_context,
          expr_ty_attr,
          expr_constraints,
          expr_unique_constraints,
          expr_substs,
          Pretyped_ast.Free (loc, expr_ty_attr, k, pretyped_expr) )
  | Weak (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun ( _,
                expr_ty_attr,
                expr_constraints,
                expr_unique_constraints,
                expr_substs,
                pretyped_expr ) ->
      Ok
        ( typing_context,
          expr_ty_attr,
          expr_constraints,
          expr_unique_constraints,
          expr_substs,
          Pretyped_ast.Weak (loc, expr_ty_attr, k, pretyped_expr) )
  | Inst (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun ( _,
                expr_ty_attr,
                expr_constraints,
                expr_unique_constraints,
                expr_substs,
                pretyped_expr ) ->
      Ok
        ( typing_context,
          expr_ty_attr,
          expr_constraints,
          expr_unique_constraints,
          expr_substs,
          Pretyped_ast.Inst (loc, expr_ty_attr, k, pretyped_expr) ))
  >>= fun ( typing_context,
            expr_ty_attr,
            expr_constraints,
            expr_unique_constraints,
            substs,
            pretyped_expr ) ->
  Pprint_type_infer.pprint_type_infer_expr_verbose Fmt.stdout ~verbose expr
    typing_context expr_ty_attr expr_constraints expr_unique_constraints;
  Ok
    ( typing_context,
      expr_ty_attr,
      expr_constraints,
      expr_unique_constraints,
      substs,
      pretyped_expr )

and generate_constraints_value_expr (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (value : Parser_ast.value)
    ~(verbose : bool) :
    (Type_infer_types.ty_attr
    * Type_infer_types.constr list
    * Type_infer_types.constr_unique list
    * Pretyped_ast.value)
    Or_error.t =
  let open Result in
  (match value with
  | Unit loc ->
      let u = fresh_unique () in
      Ok ((TyUnit, u), [], [], Pretyped_ast.Unit (loc, (TyUnit, u)))
  | Integer (loc, i) ->
      let u = fresh_unique () in
      Ok ((TyInt, u), [], [], Pretyped_ast.Integer (loc, (TyInt, u), i))
  | Boolean (loc, b) ->
      let u = fresh_unique () in
      Ok ((TyBool, u), [], [], Pretyped_ast.Boolean (loc, (TyBool, u), b))
  | Variable (loc, var) -> (
      match instantiate var typing_context with
      | Ok var_type ->
          Ok (var_type, [], [], Pretyped_ast.Variable (loc, var_type, var))
      | Error _ ->
          let function_name =
            Function_name.of_string (Var_name.to_string var)
          in
          let open Result in
          Functions_env.get_function_signature loc function_name functions_env
          >>= fun function_type_expr ->
          let ( typ_scheme_assoc_list,
                unique_scheme_assoc_list,
                type_expr_scheme_assoc_list ) =
            Type_infer_types.get_type_expr_scheme_assoc_lists
              [ function_type_expr ]
          in
          let function_ty_attr =
            convert_ast_type_to_ty_attr function_type_expr typ_scheme_assoc_list
              unique_scheme_assoc_list type_expr_scheme_assoc_list
          in
          Ok
            ( function_ty_attr,
              [],
              [],
              Pretyped_ast.Variable (loc, function_ty_attr, var) ))
  | Constructor (loc, constructor_name, constructor_values) ->
      let t = fresh () in
      let u = fresh_unique () in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (ConstructorEnvEntry
                (constructor_type, _, constructor_param_type_exprs)) ->
      Type_defns_env.get_custom_type_entry_by_name loc types_env
        constructor_type
      >>= fun (TypesEnvEntry
                (typ_poly_exprs, unique_poly_exprs, type_expr_poly_exprs, _)) ->
      let typ_scheme_assoc_list =
        List.map typ_poly_exprs ~f:(fun typ_poly_expr ->
            match typ_poly_expr with
            | TEPoly (_, Poly (_, poly)) -> (poly, fresh ())
            | _ ->
                let error_string = string_of_loc (get_typ_loc typ_poly_expr) in
                raise (PolymorphicExpected error_string))
      in
      let unique_scheme_assoc_list =
        List.map unique_poly_exprs ~f:(fun unique_poly_expr ->
            match unique_poly_expr with
            | PolyUnique (_, Poly (_, poly)) -> (poly, fresh_unique ())
            | _ ->
                let error_string =
                  string_of_loc (get_uniqueness_loc unique_poly_expr)
                in
                raise (PolymorphicExpected error_string))
      in
      let type_expr_scheme_assoc_list =
        List.map type_expr_poly_exprs ~f:(fun type_expr_poly_expr ->
            match type_expr_poly_expr with
            | TPoly (Poly (_, poly)) -> (poly, (fresh (), fresh_unique ()))
            | _ ->
                let error_string =
                  string_of_loc (get_loc type_expr_poly_expr)
                in
                raise (PolymorphicExpected error_string))
      in
      let params_contraints, params_unique_constraints, pretyped_params =
        List.fold_right2_exn constructor_values constructor_param_type_exprs
          ~init:([], [], [])
          ~f:(fun
              constructor_value
              constructor_value_type_expr
              ( acc_constraints,
                acc_unique_constraints,
                acc_pretyped_constructor_values )
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context constructor_value ~verbose
              >>= fun ( (constructor_value_ty, constructor_value_ty_unique),
                        constructor_typ_value_constraints,
                        constructor_uniqueness_value_constraints,
                        pretyped_constructor_value ) ->
                let ( converted_constructor_value_typ,
                      converted_constructor_value_uniqueness ) =
                  convert_ast_type_to_ty_attr constructor_value_type_expr
                    typ_scheme_assoc_list unique_scheme_assoc_list
                    type_expr_scheme_assoc_list
                in
                Ok
                  ( (constructor_value_ty, converted_constructor_value_typ)
                    :: constructor_typ_value_constraints
                    @ acc_constraints,
                    ( constructor_value_ty_unique,
                      converted_constructor_value_uniqueness )
                    :: constructor_uniqueness_value_constraints
                    @ acc_unique_constraints,
                    pretyped_constructor_value
                    :: acc_pretyped_constructor_values ) ))
      in
      let fresh_ty_vars_poly =
        List.map typ_scheme_assoc_list ~f:(fun (_, ty_var) -> ty_var)
      in
      let fresh_ty_unique_vars_poly =
        List.map unique_scheme_assoc_list ~f:(fun (_, ty_unique) -> ty_unique)
      in
      let fresh_ty_attr_vars_poly =
        List.map type_expr_scheme_assoc_list ~f:(fun (_, ty_attr) -> ty_attr)
      in
      Ok
        ( (t, u),
          ( t,
            TyCustom
              ( fresh_ty_vars_poly,
                fresh_ty_unique_vars_poly,
                fresh_ty_attr_vars_poly,
                constructor_type ) )
          :: params_contraints,
          params_unique_constraints,
          Pretyped_ast.Constructor
            (loc, (t, u), constructor_name, pretyped_params) ))
  >>= fun ( value_ty_attr,
            value_constraints,
            value_unique_constraints,
            pretyped_value ) ->
  Pprint_type_infer.pprint_type_infer_value_verbose Fmt.stdout ~verbose value
    value_ty_attr value_constraints value_unique_constraints;
  Ok (value_ty_attr, value_constraints, value_unique_constraints, pretyped_value)

and generate_constraints_matched_expr (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (matched_expr : Parser_ast.matched_expr)
    (get_ty_unique_from_sharing_analysis : Var_name.t -> ty_unique)
    ~(verbose : bool) :
    (typing_context
    * ty_attr
    * constr list
    * constr_unique list
    * Pretyped_ast.matched_expr)
    Or_error.t =
  match matched_expr with
  | MUnderscore loc ->
      let t = fresh () in
      let u = fresh_unique () in
      Ok ([], (t, u), [], [], Pretyped_ast.MUnderscore (loc, (t, u)))
  | MVariable (loc, matched_var_name) ->
      let t = fresh () in
      let u = fresh_unique () in
      Ok
        ( [ TypingContextEntry (matched_var_name, (t, u)) ],
          (t, u),
          [],
          [ (u, get_ty_unique_from_sharing_analysis matched_var_name) ],
          Pretyped_ast.MVariable (loc, (t, u), matched_var_name) )
  | MConstructor (loc, constructor_name, matched_exprs) ->
      let u = fresh_unique () in
      let open Result in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (Type_defns_env.ConstructorEnvEntry
                (constructor_type_name, _, constructor_arg_types)) ->
      Type_defns_env.get_custom_type_entry_by_name loc types_env
        constructor_type_name
      >>= fun (TypesEnvEntry
                (typ_poly_exprs, unique_poly_exprs, type_expr_poly_exprs, _)) ->
      let typ_scheme_assoc_list =
        List.map typ_poly_exprs ~f:(fun typ_poly_expr ->
            match typ_poly_expr with
            | TEPoly (_, Poly (_, poly)) -> (poly, fresh ())
            | _ ->
                let error_string = string_of_loc (get_typ_loc typ_poly_expr) in
                raise (PolymorphicExpected error_string))
      in
      let unique_scheme_assoc_list =
        List.map unique_poly_exprs ~f:(fun unique_poly_expr ->
            match unique_poly_expr with
            | PolyUnique (_, Poly (_, poly)) -> (poly, fresh_unique ())
            | _ ->
                let error_string =
                  string_of_loc (get_uniqueness_loc unique_poly_expr)
                in
                raise (PolymorphicExpected error_string))
      in
      let type_expr_scheme_assoc_list =
        List.map type_expr_poly_exprs ~f:(fun type_expr_poly_expr ->
            match type_expr_poly_expr with
            | TPoly (Poly (_, poly)) -> (poly, (fresh (), fresh_unique ()))
            | _ ->
                let error_string =
                  string_of_loc (get_loc type_expr_poly_expr)
                in
                raise (PolymorphicExpected error_string))
      in
      let ( matched_typing_context,
            matched_expr_constraints,
            matched_expr_unique_constraints,
            pretyped_matched_exprs ) =
        List.fold_right2_exn matched_exprs constructor_arg_types
          ~init:([], [], [], [])
          ~f:(fun
              matched_expr
              constructor_arg_type
              ( acc_matched_typing_context,
                acc_matched_expr_constraints,
                acc_matched_expr_unique_constraints,
                acc_pretyped_matched_expr )
            ->
            Or_error.ok_exn
              ( generate_constraints_matched_expr types_env constructors_env
                  matched_expr get_ty_unique_from_sharing_analysis ~verbose
              >>= fun ( matched_typing_context,
                        (matched_expr_ty, matched_expr_ty_unique),
                        matched_expr_constraints,
                        matched_expr_unique_constraints,
                        pretyped_matched_expr ) ->
                let combined_matched_typing_context =
                  Or_error.ok_exn
                    (combine_typing_contexts matched_typing_context
                       acc_matched_typing_context)
                in
                let constructor_arg_ty, constructor_arg_ty_unique =
                  convert_ast_type_to_ty_attr constructor_arg_type
                    typ_scheme_assoc_list unique_scheme_assoc_list
                    type_expr_scheme_assoc_list
                in
                Ok
                  ( combined_matched_typing_context,
                    (matched_expr_ty, constructor_arg_ty)
                    :: matched_expr_constraints
                    @ acc_matched_expr_constraints,
                    (matched_expr_ty_unique, constructor_arg_ty_unique)
                    :: matched_expr_unique_constraints
                    @ acc_matched_expr_unique_constraints,
                    pretyped_matched_expr :: acc_pretyped_matched_expr ) ))
      in
      let fresh_ty_vars_poly =
        List.map typ_scheme_assoc_list ~f:(fun (_, ty_var) -> ty_var)
      in
      let fresh_ty_unique_vars_poly =
        List.map unique_scheme_assoc_list ~f:(fun (_, ty_unique) -> ty_unique)
      in
      let fresh_ty_attr_vars_poly =
        List.map type_expr_scheme_assoc_list ~f:(fun (_, ty_attr) -> ty_attr)
      in
      Ok
        ( matched_typing_context,
          ( TyCustom
              ( fresh_ty_vars_poly,
                fresh_ty_unique_vars_poly,
                fresh_ty_attr_vars_poly,
                constructor_type_name ),
            u ),
          matched_expr_constraints,
          matched_expr_unique_constraints,
          Pretyped_ast.MConstructor
            ( loc,
              ( TyCustom
                  ( fresh_ty_vars_poly,
                    fresh_ty_unique_vars_poly,
                    fresh_ty_attr_vars_poly,
                    constructor_type_name ),
                u ),
              constructor_name,
              pretyped_matched_exprs ) )
