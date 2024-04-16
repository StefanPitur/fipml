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

module FreeSet = Set.Make (String)

let unify (constraints : constr list) : subst list Or_error.t =
  let rec unify (constraints : constr list) (substs : subst list) =
    match constraints with
    | [] -> Ok substs
    | (t1, t2) :: constraints when ty_equal t1 t2 -> unify constraints substs
    | (TyVar type_var, t) :: constraints when not (occurs type_var t) ->
        let ts = ty_subst [ (type_var, t) ] in
        unify
          (List.map constraints ~f:(fun (ty1, ty2) -> (ts ty1, ts ty2)))
          ((type_var, t) :: List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2)))
    | (t, TyVar type_var) :: constraints when not (occurs type_var t) ->
        let ts = ty_subst [ (type_var, t) ] in
        unify
          (List.map constraints ~f:(fun (ty1, ty2) -> (ts ty1, ts ty2)))
          ((type_var, t) :: List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2)))
    | (TyArrow (ty11, ty12), TyArrow (ty21, ty22)) :: constraints ->
        unify ((ty11, ty21) :: (ty12, ty22) :: constraints) substs
    | (TyTuple tys1, TyTuple tys2) :: constraints
      when List.length tys1 = List.length tys2 ->
        let tuples_constraints =
          List.map2_exn tys1 tys2 ~f:(fun ty1 ty2 -> (ty1, ty2))
        in
        unify (tuples_constraints @ constraints) substs
    | (TyCustom (tys1, type_name1), TyCustom (tys2, type_name2)) :: constraints
      when Type_name.( = ) type_name1 type_name2
           && List.length tys1 = List.length tys2 ->
        let type_scheme_vars_constraints =
          List.map2_exn tys1 tys2 ~f:(fun ty1 ty2 -> (ty1, ty2))
        in
        unify (type_scheme_vars_constraints @ constraints) substs
    | (ty1, ty2) :: _ ->
        let error_string = Fmt.str "Unable to unify types %s and %s@." in
        let string_ty1 = Pprint_type_infer.string_of_ty ty1 in
        let string_ty2 = Pprint_type_infer.string_of_ty ty2 in
        Or_error.of_exn (UnableToUnify (error_string string_ty1 string_ty2))
  in
  unify constraints []

let rec free_type_vars (ty : ty) : FreeSet.t =
  match ty with
  | TyInt | TyUnit | TyBool -> FreeSet.empty
  | TyVar ty_var -> FreeSet.singleton ty_var
  | TyPoly (polys, ty) -> Set.diff (free_type_vars ty) (FreeSet.of_list polys)
  | TyCustom (tys, _) | TyTuple tys ->
      FreeSet.union_list (List.map tys ~f:free_type_vars)
  | TyArrow (ty1, ty2) -> Set.union (free_type_vars ty1) (free_type_vars ty2)

let bounded_type_vars (typing_context : typing_context) : FreeSet.t =
  let free_tys =
    List.map typing_context ~f:(fun (TypingContextEntry (_, ty)) ->
        free_type_vars ty)
  in
  FreeSet.union_list free_tys

let rec generalise (typing_context : typing_context) (var_name : Var_name.t)
    (var_ty : ty) : typing_context Or_error.t =
  let generalisable_ty_vars =
    Set.diff (free_type_vars var_ty) (bounded_type_vars typing_context)
  in
  let poly_strings = Set.elements generalisable_ty_vars in
  extend_typing_context typing_context var_name (TyPoly (poly_strings, var_ty))

and instantiate (var_name : Var_name.t) (typing_context : typing_context) :
    ty Or_error.t =
  let open Result in
  get_var_type typing_context var_name >>= fun var_ty ->
  match var_ty with
  | TyPoly (poly_strings, ty) ->
      let substs =
        List.map poly_strings ~f:(fun poly_string -> (poly_string, fresh ()))
      in
      Ok (ty_subst substs ty)
  | ty -> Ok ty

and type_infer (types_env : types_env) (constructors_env : constructors_env)
    (functions_env : functions_env) (typing_context : typing_context)
    (expr : Parser_ast.expr) ~(verbose : bool) : Typed_ast.expr Or_error.t =
  let open Result in
  generate_constraints types_env constructors_env functions_env typing_context
    expr ~verbose
  >>= fun (_, _, constraints, let_poly_substs, pretyped_expr) ->
  unify constraints >>= fun substs ->
  Construct_typed_ast.construct_typed_ast_expr pretyped_expr
    (substs @ let_poly_substs)

and generate_constraints (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (expr : Parser_ast.expr) ~(verbose : bool)
    :
    (Type_infer_types.typing_context
    * Type_infer_types.ty
    * Type_infer_types.constr list
    * subst list
    * Pretyped_ast.expr)
    Or_error.t =
  let open Result in
  (match expr with
  | UnboxedSingleton (loc, value) ->
      generate_constraints_value_expr types_env constructors_env functions_env
        typing_context value ~verbose
      >>= fun (value_ty, value_constraints, pretyped_value) ->
      Ok
        ( typing_context,
          value_ty,
          value_constraints,
          [],
          Pretyped_ast.UnboxedSingleton (loc, value_ty, pretyped_value) )
  | UnboxedTuple (loc, values) ->
      let values_types, values_constraints, values_pretyped =
        List.fold_right values ~init:([], [], [])
          ~f:(fun value (acc_types, acc_constraints, acc_pretyped_values) ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context value ~verbose
              >>= fun (value_ty, value_constraints, pretyped_value) ->
                Ok
                  ( value_ty :: acc_types,
                    value_constraints @ acc_constraints,
                    pretyped_value :: acc_pretyped_values ) ))
      in
      Ok
        ( typing_context,
          TyTuple values_types,
          values_constraints,
          [],
          Pretyped_ast.UnboxedTuple (loc, TyTuple values_types, values_pretyped)
        )
  | Let (loc, var_names, vars_expr, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context vars_expr ~verbose
      >>= fun (_, var_type, var_constrs, var_pretyped_substs, pretyped_vars) ->
      unify var_constrs >>= fun var_substs ->
      let sub_var_type = ty_subst var_substs var_type in
      let sub_typing_context = ty_subst_context typing_context var_substs in
      let extended_typing_context =
        Or_error.ok_exn
          (match sub_var_type with
          | TyTuple tys ->
              if List.length var_names = 1 then Or_error.of_exn TyNotMatching
              else
                Ok
                  (List.fold2_exn var_names tys ~init:sub_typing_context
                     ~f:(fun typing_context var_name var_type ->
                       Or_error.ok_exn
                         (generalise typing_context var_name var_type)))
          | _ -> (
              match var_names with
              | [ var_name ] ->
                  Ok
                    (Or_error.ok_exn
                       (generalise sub_typing_context var_name sub_var_type))
              | _ -> Or_error.of_exn TyNotMatching))
      in

      (* Fmt.pf Fmt.stdout "Let-Poly@.";
         Fmt.pf Fmt.stdout "sub_var_type = %s@." (Pprint_type_infer.string_of_ty sub_var_type);
         Pprint_type_infer.pprint_typing_context Fmt.stdout extended_typing_context;
         Fmt.pf Fmt.stdout "FIND ME\n===================@."; *)
      generate_constraints types_env constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun (_, expr_type, expr_constrs, expr_substs, pretyped_expr) ->
      Ok
        ( extended_typing_context,
          expr_type,
          expr_constrs,
          var_pretyped_substs @ var_substs @ expr_substs,
          Pretyped_ast.Let
            (loc, expr_type, var_names, pretyped_vars, pretyped_expr) )
  | FunApp (loc, app_var_name, app_values) ->
      get_var_type typing_context app_var_name >>= fun app_var_ty ->
      get_ty_function_signature app_var_ty
      >>= fun (app_params_tys, app_return_ty) ->
      let app_params_constraints, app_params_pretyped =
        List.fold_right2_exn app_params_tys app_values ~init:([], [])
          ~f:(fun
              app_param_ty
              app_value
              (acc_params_constraints, acc_params_pretyped)
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context app_value ~verbose
              >>= fun (value_ty, value_constraints, pretyped_value) ->
                Ok
                  ( ((value_ty, app_param_ty) :: value_constraints)
                    @ acc_params_constraints,
                    pretyped_value :: acc_params_pretyped ) ))
      in
      Ok
        ( typing_context,
          app_return_ty,
          app_params_constraints,
          [],
          Pretyped_ast.FunApp
            (loc, app_return_ty, app_var_name, app_params_pretyped) )
  | FunCall (loc, function_name, values) ->
      Functions_env.get_function_by_name loc function_name functions_env
      >>= fun (FunctionEnvEntry
                (_, _, _, function_args_types, function_return_type)) ->
      if List.length values <> List.length function_args_types then
        Or_error.of_exn PartialFunctionApplicationNotAllowed
      else
        let args_constraints, args_pretyped =
          List.fold_right2_exn function_args_types values ~init:([], [])
            ~f:(fun
                function_arg_type
                value
                (acc_constraints, acc_pretyped_values)
              ->
              Or_error.ok_exn
                ( generate_constraints_value_expr types_env constructors_env
                    functions_env typing_context value ~verbose
                >>= fun (value_ty, value_constraints, pretyped_value) ->
                  Ok
                    ( (convert_ast_type_to_ty function_arg_type [], value_ty)
                      :: value_constraints
                      @ acc_constraints,
                      pretyped_value :: acc_pretyped_values ) ))
        in
        Ok
          ( typing_context,
            convert_ast_type_to_ty function_return_type [],
            args_constraints,
            [],
            Pretyped_ast.FunCall
              ( loc,
                convert_ast_type_to_ty function_return_type [],
                function_name,
                args_pretyped ) )
  | If (loc, expr_cond, expr_then) ->
      let t = fresh () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr_cond ~verbose
      >>= fun ( _,
                expr_cond_type,
                expr_cond_constrs,
                expr_cond_substs,
                pretyped_expr ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun ( _,
                expr_then_type,
                expr_then_constrs,
                expr_then_substs,
                pretyped_then_expr ) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type) ]
          @ expr_cond_constrs @ expr_then_constrs,
          expr_cond_substs @ expr_then_substs,
          Pretyped_ast.If (loc, t, pretyped_expr, pretyped_then_expr) )
  | IfElse (loc, expr_cond, expr_then, expr_else) ->
      let t = fresh () in
      generate_constraints types_env constructors_env functions_env
        typing_context expr_cond ~verbose
      >>= fun ( _,
                expr_cond_type,
                expr_cond_constrs,
                expr_cond_substs,
                pretyped_expr ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun ( _,
                expr_then_type,
                expr_then_constrs,
                expr_then_substs,
                pretyped_expr_then ) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr_else ~verbose
      >>= fun ( _,
                expr_else_type,
                expr_else_constrs,
                expr_else_substs,
                pretyped_expr_else ) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type); (t, expr_else_type) ]
          @ expr_cond_constrs @ expr_then_constrs @ expr_else_constrs,
          expr_cond_substs @ expr_then_substs @ expr_else_substs,
          Pretyped_ast.IfElse
            (loc, t, pretyped_expr, pretyped_expr_then, pretyped_expr_else) )
  | Match (loc, var_name, pattern_exprs) ->
      let t = fresh () in
      instantiate var_name typing_context >>= fun matched_var_type ->
      Ok
        (List.fold_right pattern_exprs ~init:([], [], [])
           ~f:(fun
               (MPattern (loc, matched_expr, pattern_expr))
               (acc_constraints, acc_substs, acc_pretyped_pattern_exprs)
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr types_env constructors_env
                   matched_expr ~verbose
               >>= fun ( matched_typing_context,
                         matched_expr_type,
                         match_expr_constraints,
                         pretyped_matched_expr ) ->
                 let combined_typing_contexts =
                   Or_error.ok_exn
                     (combine_typing_contexts matched_typing_context
                        typing_context)
                 in
                 generate_constraints types_env constructors_env functions_env
                   combined_typing_contexts pattern_expr ~verbose
                 >>= fun ( _,
                           pattern_expr_type,
                           pattern_expr_constraints,
                           pattern_expr_substs,
                           pretyped_pattern_expr ) ->
                 Ok
                   ( (matched_var_type, matched_expr_type)
                     :: (t, pattern_expr_type) :: match_expr_constraints
                     @ pattern_expr_constraints @ acc_constraints,
                     pattern_expr_substs @ acc_substs,
                     Pretyped_ast.MPattern
                       ( loc,
                         pattern_expr_type,
                         pretyped_matched_expr,
                         pretyped_pattern_expr )
                     :: acc_pretyped_pattern_exprs ) )))
      >>= fun (match_expr_constraints, match_expr_substs, pretyped_pattern_exprs)
        ->
      Ok
        ( typing_context,
          t,
          match_expr_constraints,
          match_expr_substs,
          Pretyped_ast.Match (loc, t, var_name, pretyped_pattern_exprs) )
  | UnOp (loc, unary_op, expr) -> (
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun (_, expr_type, expr_constrs, expr_substs, pretyped_expr) ->
      match unary_op with
      | UnOpNeg ->
          Ok
            ( typing_context,
              expr_type,
              (expr_type, TyInt) :: expr_constrs,
              expr_substs,
              Pretyped_ast.UnOp (loc, expr_type, UnOpNeg, pretyped_expr) )
      | UnOpNot ->
          Ok
            ( typing_context,
              expr_type,
              (expr_type, TyBool) :: expr_constrs,
              expr_substs,
              Pretyped_ast.UnOp (loc, expr_type, UnOpNot, pretyped_expr) ))
  | BinaryOp (loc, binary_op, expr1, expr2) -> (
      generate_constraints types_env constructors_env functions_env
        typing_context expr1 ~verbose
      >>= fun (_, expr1_type, expr1_constrs, expr1_substs, pretyped_expr1) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr2 ~verbose
      >>= fun (_, expr2_type, expr2_constrs, expr2_substs, pretyped_expr2) ->
      match binary_op with
      | (BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpMod) as binary_op
        ->
          Ok
            ( typing_context,
              TyInt,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              expr1_substs @ expr2_substs,
              Pretyped_ast.BinaryOp
                (loc, TyInt, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpLt | BinOpGt | BinOpLeq | BinOpGeq) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              expr1_substs @ expr2_substs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpEq | BinOpNeq) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, expr2_type) :: expr1_constrs) @ expr2_constrs,
              expr1_substs @ expr2_substs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpAnd | BinOpOr) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyBool) :: (expr2_type, TyBool) :: expr1_constrs)
              @ expr2_constrs,
              expr1_substs @ expr2_substs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) ))
  | Drop (loc, var_name, expr) ->
      remove_var_from_typing_context typing_context var_name
      >>= fun extended_typing_context ->
      generate_constraints types_env constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun (_, expr_ty, expr_constraints, expr_substs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          expr_constraints,
          expr_substs,
          Pretyped_ast.Drop (loc, TyUnit, var_name, pretyped_expr) )
  | Free (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun (_, expr_ty, expr_ty_constraints, expr_substs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          expr_ty_constraints,
          expr_substs,
          Pretyped_ast.Free (loc, expr_ty, k, pretyped_expr) )
  | Weak (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun (_, expr_ty, expr_ty_constraints, expr_substs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          expr_ty_constraints,
          expr_substs,
          Pretyped_ast.Weak (loc, expr_ty, k, pretyped_expr) )
  | Inst (loc, k, expr) ->
      generate_constraints types_env constructors_env functions_env
        typing_context expr ~verbose
      >>= fun (_, expr_ty, expr_ty_constraints, expr_substs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          expr_ty_constraints,
          expr_substs,
          Pretyped_ast.Inst (loc, expr_ty, k, pretyped_expr) ))
  >>= fun (typing_context, expr_tys, expr_constraints, substs, pretyped_expr) ->
  Pprint_type_infer.pprint_type_infer_expr_verbose Fmt.stdout ~verbose expr
    typing_context expr_tys expr_constraints;
  Ok (typing_context, expr_tys, expr_constraints, substs, pretyped_expr)

and generate_constraints_value_expr (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (value : Parser_ast.value)
    ~(verbose : bool) :
    (Type_infer_types.ty * Type_infer_types.constr list * Pretyped_ast.value)
    Or_error.t =
  let open Result in
  (match value with
  | Unit loc -> Ok (TyUnit, [], Pretyped_ast.Unit (loc, TyUnit))
  | Integer (loc, i) -> Ok (TyInt, [], Pretyped_ast.Integer (loc, TyInt, i))
  | Boolean (loc, b) -> Ok (TyBool, [], Pretyped_ast.Boolean (loc, TyBool, b))
  | Variable (loc, var) -> (
      match instantiate var typing_context with
      | Ok var_type ->
          Ok (var_type, [], Pretyped_ast.Variable (loc, var_type, var))
      | Error _ ->
          let function_name =
            Function_name.of_string (Var_name.to_string var)
          in
          let open Result in
          Functions_env.get_function_signature loc function_name functions_env
          >>= fun function_type ->
          let function_ty = convert_ast_type_to_ty function_type [] in
          Ok (function_ty, [], Pretyped_ast.Variable (loc, function_ty, var)))
  | Constructor (loc, constructor_name, constructor_values) ->
      let t = fresh () in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (ConstructorEnvEntry
                (constructor_type, _, constructor_param_types)) ->
      Type_defns_env.get_custom_type_entry_by_name loc types_env
        constructor_type
      >>= fun (TypesEnvEntry (type_scheme_poly_exprs, _)) ->
      get_type_scheme_assoc_list type_scheme_poly_exprs
      >>= fun type_scheme_assoc_list ->
      let params_contraints, pretyped_params =
        List.fold_right2_exn constructor_values constructor_param_types
          ~init:([], [])
          ~f:(fun
              constructor_value
              constructor_value_type
              (acc_constraints, acc_pretyped_constructor_values)
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr types_env constructors_env
                  functions_env typing_context constructor_value ~verbose
              >>= fun ( constructor_value_ty,
                        constructor_value_constraints,
                        pretyped_constructor_value ) ->
                Ok
                  ( ( constructor_value_ty,
                      convert_ast_type_to_ty constructor_value_type
                        type_scheme_assoc_list )
                    :: constructor_value_constraints
                    @ acc_constraints,
                    pretyped_constructor_value
                    :: acc_pretyped_constructor_values ) ))
      in
      let fresh_ty_vars_poly =
        List.map type_scheme_assoc_list ~f:(fun (_, ty_var) -> ty_var)
      in
      Ok
        ( t,
          (t, TyCustom (fresh_ty_vars_poly, constructor_type))
          :: params_contraints,
          Pretyped_ast.Constructor (loc, t, constructor_name, pretyped_params)
        ))
  >>= fun (value_ty, value_constraints, pretyped_value) ->
  Pprint_type_infer.pprint_type_infer_value_verbose Fmt.stdout ~verbose value
    value_ty value_constraints;
  Ok (value_ty, value_constraints, pretyped_value)

and generate_constraints_matched_expr (types_env : types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (matched_expr : Parser_ast.matched_expr) ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.matched_expr) Or_error.t =
  match matched_expr with
  | MUnderscore loc ->
      let t = fresh () in
      Ok ([], t, [], Pretyped_ast.MUnderscore (loc, t))
  | MVariable (loc, matched_var_name) ->
      let t = fresh () in
      Ok
        ( [ TypingContextEntry (matched_var_name, t) ],
          t,
          [],
          Pretyped_ast.MVariable (loc, t, matched_var_name) )
  | MConstructor (loc, constructor_name, matched_exprs) ->
      let open Result in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (Type_defns_env.ConstructorEnvEntry
                (constructor_type_name, _, constructor_arg_types)) ->
      Type_defns_env.get_custom_type_entry_by_name loc types_env
        constructor_type_name
      >>= fun (TypesEnvEntry (type_scheme_poly_exprs, _)) ->
      get_type_scheme_assoc_list type_scheme_poly_exprs
      >>= fun type_scheme_assoc_list ->
      let ( matched_typing_context,
            matched_expr_constraints,
            pretyped_matched_exprs ) =
        List.fold_right2_exn matched_exprs constructor_arg_types
          ~init:([], [], [])
          ~f:(fun
              matched_expr
              matched_expr_type
              ( acc_matched_typing_context,
                acc_matched_expr_constraints,
                acc_pretyped_matched_expr )
            ->
            Or_error.ok_exn
              ( generate_constraints_matched_expr types_env constructors_env
                  matched_expr ~verbose
              >>= fun ( matched_typing_context,
                        matched_expr_ty,
                        matched_expr_constraints,
                        pretyped_matched_expr ) ->
                let combined_matched_typing_context =
                  Or_error.ok_exn
                    (combine_typing_contexts matched_typing_context
                       acc_matched_typing_context)
                in
                Ok
                  ( combined_matched_typing_context,
                    ( matched_expr_ty,
                      convert_ast_type_to_ty matched_expr_type
                        type_scheme_assoc_list )
                    :: matched_expr_constraints
                    @ acc_matched_expr_constraints,
                    pretyped_matched_expr :: acc_pretyped_matched_expr ) ))
      in
      let fresh_ty_vars_poly =
        List.map type_scheme_assoc_list ~f:(fun (_, ty_var) -> ty_var)
      in
      Ok
        ( matched_typing_context,
          TyCustom (fresh_ty_vars_poly, constructor_type_name),
          matched_expr_constraints,
          Pretyped_ast.MConstructor
            ( loc,
              TyCustom (fresh_ty_vars_poly, constructor_type_name),
              constructor_name,
              pretyped_matched_exprs ) )
