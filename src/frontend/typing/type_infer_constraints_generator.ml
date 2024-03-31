open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Type_infer_types

exception TyNotMatching
exception PartialFunctionApplicationNotAllowed

let rec generate_constraints
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (expr : expr) ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.expr) Or_error.t =
  let open Result in
  (match expr with
  | UnboxedSingleton (loc, value) ->
      generate_constraints_value_expr constructors_env typing_context value
        ~verbose
      >>= fun (value_ty, value_constraints, pretyped_value) ->
      Ok
        ( typing_context,
          value_ty,
          value_constraints,
          Pretyped_ast.UnboxedSingleton (loc, value_ty, pretyped_value) )
  | UnboxedTuple (loc, values) ->
      let values_types, values_constraints, values_pretyped =
        List.fold_right values ~init:([], [], [])
          ~f:(fun value (acc_types, acc_constraints, acc_pretyped_values) ->
            Or_error.ok_exn
              ( generate_constraints_value_expr constructors_env typing_context
                  value ~verbose
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
          Pretyped_ast.UnboxedTuple (loc, TyTuple values_types, values_pretyped)
        )
  (* Note that we do not care about Polymorphic - Let (at the moment...), as we only allow top-level functions *)
  | Let (loc, var_names, vars_expr, expr) ->
      generate_constraints constructors_env functions_env typing_context
        vars_expr ~verbose
      >>= fun (_, var_type, var_constrs, pretyped_vars) ->
      let extended_typing_context, extra_tyvar_constraints =
        Or_error.ok_exn
          (match var_type with
          | TyTuple tys ->
              if List.length var_names = 1 then Or_error.of_exn TyNotMatching
              else
                Ok
                  ( List.fold2_exn var_names tys ~init:typing_context
                      ~f:(fun typing_context var_name var_type ->
                        Or_error.ok_exn
                          (Type_context_env.extend_typing_context typing_context
                             var_name var_type)),
                    [] )
          | TyVar _ when List.length var_names <> 1 ->
              let typing_context, fresh_tys =
                List.fold_right var_names ~init:(typing_context, [])
                  ~f:(fun var_name (typing_context, fresh_tys) ->
                    let fresh_ty = fresh () in
                    ( Or_error.ok_exn
                        (Type_context_env.extend_typing_context typing_context
                           var_name fresh_ty),
                      fresh_ty :: fresh_tys ))
              in
              Ok (typing_context, [ (TyTuple fresh_tys, var_type) ])
          | _ -> (
              match var_names with
              | [ var_name ] ->
                  Ok
                    ( Or_error.ok_exn
                        (Type_context_env.extend_typing_context typing_context
                           var_name var_type),
                      [] )
              | _ -> Or_error.of_exn TyNotMatching))
      in
      generate_constraints constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun (_, expr_type, expr_constrs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_type,
          expr_constrs @ extra_tyvar_constraints @ var_constrs,
          Pretyped_ast.Let
            (loc, expr_type, var_names, pretyped_vars, pretyped_expr) )
  | FunApp (loc, app_var_name, app_exprs) ->
      Type_context_env.get_var_type typing_context app_var_name
      >>= fun app_var_ty ->
      get_ty_function_signature app_var_ty
      >>= fun (app_params_tys, app_return_ty) ->
      let app_params_constraints, app_params_pretyped =
        List.fold_right2_exn app_params_tys app_exprs ~init:([], [])
          ~f:(fun
              app_param_ty
              app_expr
              (acc_params_constraints, acc_params_pretyped)
            ->
            Or_error.ok_exn
              ( generate_constraints constructors_env functions_env
                  typing_context app_expr ~verbose
              >>= fun (_, expr_ty, expr_constraints, pretyped_expr) ->
                Ok
                  ( ((expr_ty, app_param_ty) :: expr_constraints)
                    @ acc_params_constraints,
                    pretyped_expr :: acc_params_pretyped ) ))
      in
      Ok
        ( typing_context,
          app_return_ty,
          app_params_constraints,
          Pretyped_ast.FunApp
            (loc, app_return_ty, app_var_name, app_params_pretyped) )
  | FunCall (loc, function_name, exprs) ->
      Functions_env.get_function_by_name loc function_name functions_env
      >>= fun (FunctionEnvEntry (_, function_args_types, function_return_type))
        ->
      if List.length exprs <> List.length function_args_types then
        Or_error.of_exn PartialFunctionApplicationNotAllowed
      else
        let args_constraints, args_pretyped =
          List.fold_right2_exn function_args_types exprs ~init:([], [])
            ~f:(fun
                function_arg_type expr (acc_constraints, acc_pretyped_exprs) ->
              Or_error.ok_exn
                ( generate_constraints constructors_env functions_env
                    typing_context expr ~verbose
                >>= fun (_, expr_ty, expr_constraints, pretyped_expr) ->
                  Ok
                    ( (convert_ast_type_to_ty function_arg_type, expr_ty)
                      :: expr_constraints
                      @ acc_constraints,
                      pretyped_expr :: acc_pretyped_exprs ) ))
        in
        Ok
          ( typing_context,
            convert_ast_type_to_ty function_return_type,
            args_constraints,
            Pretyped_ast.FunCall
              ( loc,
                convert_ast_type_to_ty function_return_type,
                function_name,
                args_pretyped ) )
  | If (loc, expr_cond, expr_then) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond ~verbose
      >>= fun (_, expr_cond_type, expr_cond_constrs, pretyped_expr) ->
      generate_constraints constructors_env functions_env typing_context
        expr_then ~verbose
      >>= fun (_, expr_then_type, expr_then_constrs, pretyped_then_expr) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type) ]
          @ expr_cond_constrs @ expr_then_constrs,
          Pretyped_ast.If (loc, t, pretyped_expr, pretyped_then_expr) )
  | IfElse (loc, expr_cond, expr_then, expr_else) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond ~verbose
      >>= fun (_, expr_cond_type, expr_cond_constrs, pretyped_expr) ->
      generate_constraints constructors_env functions_env typing_context
        expr_then ~verbose
      >>= fun (_, expr_then_type, expr_then_constrs, pretyped_expr_then) ->
      generate_constraints constructors_env functions_env typing_context
        expr_else ~verbose
      >>= fun (_, expr_else_type, expr_else_constrs, pretyped_expr_else) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type); (t, expr_else_type) ]
          @ expr_cond_constrs @ expr_then_constrs @ expr_else_constrs,
          Pretyped_ast.IfElse
            (loc, t, pretyped_expr, pretyped_expr_then, pretyped_expr_else) )
  | Match (loc, var_name, pattern_exprs) ->
      let t = fresh () in
      Type_context_env.get_var_type typing_context var_name
      >>= fun matched_var_type ->
      Ok
        (List.fold_right pattern_exprs ~init:([], [])
           ~f:(fun
               (MPattern (loc, matched_expr, pattern_expr))
               (acc_constraints, acc_pretyped_pattern_exprs)
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env matched_expr
                   ~verbose
               >>= fun ( matched_typing_context,
                         matched_expr_type,
                         match_expr_constraints,
                         pretyped_matched_expr ) ->
                 let combined_typing_contexts =
                   Or_error.ok_exn
                     (Type_context_env.combine_typing_contexts
                        matched_typing_context typing_context)
                 in
                 generate_constraints constructors_env functions_env
                   combined_typing_contexts pattern_expr ~verbose
                 >>= fun ( _,
                           pattern_expr_type,
                           pattern_expr_constraints,
                           pretyped_pattern_expr ) ->
                 Ok
                   ( (matched_var_type, matched_expr_type)
                     :: (t, pattern_expr_type) :: match_expr_constraints
                     @ pattern_expr_constraints @ acc_constraints,
                     Pretyped_ast.MPattern
                       ( loc,
                         pattern_expr_type,
                         pretyped_matched_expr,
                         pretyped_pattern_expr )
                     :: acc_pretyped_pattern_exprs ) )))
      >>= fun (match_expr_constraints, pretyped_pattern_exprs) ->
      Ok
        ( typing_context,
          t,
          match_expr_constraints,
          Pretyped_ast.Match (loc, t, var_name, pretyped_pattern_exprs) )
  | UnOp (loc, unary_op, expr) -> (
      generate_constraints constructors_env functions_env typing_context expr
        ~verbose
      >>= fun (_, expr_type, expr_constrs, pretyped_expr) ->
      match unary_op with
      | UnOpNeg ->
          Ok
            ( typing_context,
              expr_type,
              (expr_type, TyInt) :: expr_constrs,
              Pretyped_ast.UnOp (loc, expr_type, UnOpNeg, pretyped_expr) )
      | UnOpNot ->
          Ok
            ( typing_context,
              expr_type,
              (expr_type, TyBool) :: expr_constrs,
              Pretyped_ast.UnOp (loc, expr_type, UnOpNot, pretyped_expr) ))
  | BinaryOp (loc, binary_op, expr1, expr2) -> (
      generate_constraints constructors_env functions_env typing_context expr1
        ~verbose
      >>= fun (_, expr1_type, expr1_constrs, pretyped_expr1) ->
      generate_constraints constructors_env functions_env typing_context expr2
        ~verbose
      >>= fun (_, expr2_type, expr2_constrs, pretyped_expr2) ->
      match binary_op with
      | (BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpMod) as binary_op
        ->
          Ok
            ( typing_context,
              TyInt,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              Pretyped_ast.BinaryOp
                (loc, TyInt, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpLt | BinOpGt | BinOpLeq | BinOpGeq) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpEq | BinOpNeq) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, expr2_type) :: expr1_constrs) @ expr2_constrs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) )
      | (BinOpAnd | BinOpOr) as binary_op ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyBool) :: (expr2_type, TyBool) :: expr1_constrs)
              @ expr2_constrs,
              Pretyped_ast.BinaryOp
                (loc, TyBool, binary_op, pretyped_expr1, pretyped_expr2) ))
  | Drop (loc, var_name, expr) ->
      Type_context_env.remove_var_from_typing_context typing_context var_name
      >>= fun extended_typing_context ->
      generate_constraints constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun (_, expr_ty, expr_constraints, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          expr_constraints,
          Pretyped_ast.Drop (loc, TyUnit, var_name, pretyped_expr) )
  | Free (loc, value, expr) ->
      generate_constraints_value_expr constructors_env typing_context value
        ~verbose
      >>= fun (free_ty, free_ty_constraints, pretyped_value) ->
      generate_constraints constructors_env functions_env typing_context expr
        ~verbose
      >>= fun (_, expr_ty, expr_ty_constraints, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_ty,
          ((free_ty, TyInt) :: free_ty_constraints) @ expr_ty_constraints,
          Pretyped_ast.Free (loc, expr_ty, pretyped_value, pretyped_expr) ))
  >>= fun (typing_context, expr_tys, expr_constraints, pretyped_expr) ->
  Pprint_type_infer.pprint_type_infer_expr_verbose Fmt.stdout ~verbose expr
    typing_context expr_tys expr_constraints;
  Ok (typing_context, expr_tys, expr_constraints, pretyped_expr)

and generate_constraints_value_expr
    (constructors_env : Type_defns_env.constructors_env)
    (typing_context : typing_context) (value : value) ~(verbose : bool) :
    (ty * constr list * Pretyped_ast.value) Or_error.t =
  let open Result in
  (match value with
  | Unit loc -> Ok (TyUnit, [], Pretyped_ast.Unit (loc, TyUnit))
  | Integer (loc, i) -> Ok (TyInt, [], Pretyped_ast.Integer (loc, TyInt, i))
  | Boolean (loc, b) -> Ok (TyBool, [], Pretyped_ast.Boolean (loc, TyBool, b))
  | Variable (loc, var) ->
      Type_context_env.get_var_type typing_context var >>= fun var_type ->
      Ok (var_type, [], Pretyped_ast.Variable (loc, var_type, var))
  | Constructor (loc, constructor_name, constructor_values) ->
      let t = fresh () in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (ConstructorEnvEntry
                (constructor_type, _, constructor_param_types)) ->
      let params_contraints, pretyped_params =
        List.fold_right2_exn constructor_values constructor_param_types
          ~init:([], [])
          ~f:(fun
              constructor_value
              constructor_value_type
              (acc_constraints, acc_pretyped_constructor_values)
            ->
            Or_error.ok_exn
              ( generate_constraints_value_expr constructors_env typing_context
                  constructor_value ~verbose
              >>= fun ( constructor_value_ty,
                        constructor_value_constraints,
                        pretyped_constructor_value ) ->
                Ok
                  ( ( constructor_value_ty,
                      convert_ast_type_to_ty constructor_value_type )
                    :: constructor_value_constraints
                    @ acc_constraints,
                    pretyped_constructor_value
                    :: acc_pretyped_constructor_values ) ))
      in
      Ok
        ( t,
          (t, TyCustom constructor_type) :: params_contraints,
          Pretyped_ast.Constructor (loc, t, constructor_name, pretyped_params)
        ))
  >>= fun (value_ty, value_constraints, pretyped_value) ->
  Pprint_type_infer.pprint_type_infer_value_verbose Fmt.stdout ~verbose value
    value_ty value_constraints;
  Ok (value_ty, value_constraints, pretyped_value)

and generate_constraints_matched_expr
    (constructors_env : Type_defns_env.constructors_env)
    (matched_expr : matched_expr) ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.matched_expr) Or_error.t =
  match matched_expr with
  | MUnderscore loc ->
      let t = fresh () in
      Ok ([], t, [], Pretyped_ast.MUnderscore (loc, t))
  | MVariable (loc, matched_var_name) ->
      let t = fresh () in
      Ok
        ( [ Type_context_env.TypingContextEntry (matched_var_name, t) ],
          t,
          [],
          Pretyped_ast.MVariable (loc, t, matched_var_name) )
  | MConstructor (loc, constructor_name, matched_exprs) ->
      let open Result in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (Type_defns_env.ConstructorEnvEntry
                (constructor_type_name, _, constructor_arg_types)) ->
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
              ( generate_constraints_matched_expr constructors_env matched_expr
                  ~verbose
              >>= fun ( matched_typing_context,
                        matched_expr_ty,
                        matched_expr_constraints,
                        pretyped_matched_expr ) ->
                let combined_matched_typing_context =
                  Or_error.ok_exn
                    (Type_context_env.combine_typing_contexts
                       matched_typing_context acc_matched_typing_context)
                in
                Ok
                  ( combined_matched_typing_context,
                    (matched_expr_ty, convert_ast_type_to_ty matched_expr_type)
                    :: matched_expr_constraints
                    @ acc_matched_expr_constraints,
                    pretyped_matched_expr :: acc_pretyped_matched_expr ) ))
      in
      Ok
        ( matched_typing_context,
          TyCustom constructor_type_name,
          matched_expr_constraints,
          Pretyped_ast.MConstructor
            ( loc,
              TyCustom constructor_type_name,
              constructor_name,
              pretyped_matched_exprs ) )
