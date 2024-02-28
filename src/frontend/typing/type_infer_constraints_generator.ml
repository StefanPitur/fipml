open Ast.Ast_types
open Core
open Parsing.Parser_ast
open Type_infer_types

(*
  Implementation notes:
  - nothing on tuples actually works, need to implement tuples  
  - typing context is not passed between successive expressions in block_expr 
*)

let rec generate_constraints_block_expr
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context)
    (Block (loc, exprs) as block_expr : block_expr) ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.block_expr) Or_error.t =
  let open Result in
  pop_last_element_from_list exprs >>= fun (last_expr, exprs) ->
  Ok
    (List.fold_left exprs ~init:([], [])
       ~f:(fun (acc_constraints, acc_pretyped_exprs) expr ->
         Or_error.ok_exn
           ( generate_constraints constructors_env functions_env typing_context
               expr ~verbose
           >>= fun (_, expr_type, expr_constraints, pretyped_expr) ->
             Ok
               ( ((expr_type, TyUnit) :: expr_constraints) @ acc_constraints,
                 pretyped_expr :: acc_pretyped_exprs ) )))
  >>= fun (block_expr_constraints, pretyped_block_exprs) ->
  generate_constraints constructors_env functions_env typing_context last_expr
    ~verbose
  >>= fun (_, block_type, last_expr_constraints, pretyped_last_expr) ->
  let block_expr_constraints = last_expr_constraints @ block_expr_constraints in
  Pprint_type_infer.pprint_type_infer_block_expr_verbose Fmt.stdout ~verbose
    block_expr typing_context block_type block_expr_constraints;
  Ok
    ( typing_context,
      block_type,
      block_expr_constraints,
      Pretyped_ast.Block
        (loc, block_type, pretyped_block_exprs @ [ pretyped_last_expr ]) )

and generate_constraints (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (expr : expr) ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.expr) Or_error.t =
  let open Result in
  (match expr with
  | Unit loc -> Ok (typing_context, TyUnit, [], Pretyped_ast.Unit (loc, TyUnit))
  | Integer (loc, i) ->
      Ok (typing_context, TyInt, [], Pretyped_ast.Integer (loc, TyInt, i))
  | Boolean (loc, b) ->
      Ok (typing_context, TyBool, [], Pretyped_ast.Boolean (loc, TyBool, b))
  | Option (loc, expr) -> (
      let t = fresh () in
      match expr with
      | None -> Ok (typing_context, t, [], Pretyped_ast.Option (loc, t, None))
      | Some expr ->
          generate_constraints constructors_env functions_env typing_context
            expr ~verbose
          >>= fun (_, expr_type, expr_constrs, pretyped_expr) ->
          Ok
            ( typing_context,
              t,
              (t, TyOption expr_type) :: expr_constrs,
              Pretyped_ast.Option (loc, t, Some pretyped_expr) ))
  | Variable (loc, var) ->
      Type_context_env.get_var_type typing_context var >>= fun var_type ->
      Ok
        ( typing_context,
          var_type,
          [],
          Pretyped_ast.Variable (loc, var_type, var) )
  | Constructor (loc, constructor_name, constructor_exprs) ->
      let t = fresh () in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (ConstructorEnvEntry
                (constructor_type, _, constructor_param_types)) ->
      zip_lists constructor_exprs constructor_param_types
      (* Maybe this should be a fold_right, based on how we determine pretyped_params *)
      >>| List.fold_left ~init:([], [])
            ~f:(fun
                (acc_constraints, acc_pretyped_params)
                (constructor_expr, constructor_param_type)
              ->
              Or_error.ok_exn
                ( generate_constraints constructors_env functions_env
                    typing_context constructor_expr ~verbose
                >>= fun (_, param_type, param_constraints, pretyped_param) ->
                  Ok
                    ( (param_type, convert_ast_type_to_ty constructor_param_type)
                      :: param_constraints
                      @ acc_constraints,
                      acc_pretyped_params @ [ pretyped_param ] ) ))
      >>= fun (params_contraints, pretyped_params) ->
      Ok
        ( typing_context,
          t,
          (t, TyCustom constructor_type) :: params_contraints,
          Pretyped_ast.Constructor (loc, t, constructor_name, pretyped_params)
        )
  (* TODO: Implement Tuples, they're not in the language right now as a type*)
  | Tuple (loc, _, _) ->
      Ok
        ( typing_context,
          TyCustom (Type_name.of_string "_undefined"),
          [],
          Pretyped_ast.Unit (loc, TyCustom (Type_name.of_string "_undefined"))
        )
  (* Note that we do not care about Polymorphic - Let, as we only allow top-level functions *)
  | Let (loc, var_name, var_expr, expr) ->
      generate_constraints constructors_env functions_env typing_context
        var_expr ~verbose
      >>= fun (_, var_type, var_constrs, pretyped_var) ->
      let extended_typing_context =
        Type_context_env.extend_typing_context typing_context var_name var_type
      in
      generate_constraints constructors_env functions_env
        extended_typing_context expr ~verbose
      >>= fun (_, expr_type, expr_constrs, pretyped_expr) ->
      Ok
        ( typing_context,
          expr_type,
          expr_constrs @ var_constrs,
          Pretyped_ast.Let
            (loc, var_type, var_name, pretyped_var, expr_type, pretyped_expr) )
  | If (loc, expr_cond, expr_then) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond ~verbose
      >>= fun (_, expr_cond_type, expr_cond_constrs, pretyped_expr) ->
      generate_constraints_block_expr constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun (_, expr_then_type, expr_then_constrs, pretyped_block_expr) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type) ]
          @ expr_cond_constrs @ expr_then_constrs,
          Pretyped_ast.If (loc, t, pretyped_expr, pretyped_block_expr) )
  | IfElse (loc, expr_cond, expr_then, expr_else) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond ~verbose
      >>= fun (_, expr_cond_type, expr_cond_constrs, pretyped_expr) ->
      generate_constraints_block_expr constructors_env functions_env
        typing_context expr_then ~verbose
      >>= fun (_, expr_then_type, expr_then_constrs, pretyped_block_expr_then)
        ->
      generate_constraints_block_expr constructors_env functions_env
        typing_context expr_else ~verbose
      >>= fun (_, expr_else_type, expr_else_constrs, pretyped_block_expr_else)
        ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type); (t, expr_else_type) ]
          @ expr_cond_constrs @ expr_then_constrs @ expr_else_constrs,
          Pretyped_ast.IfElse
            ( loc,
              t,
              pretyped_expr,
              pretyped_block_expr_then,
              pretyped_block_expr_else ) )
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
              Pretyped_ast.UnOp (loc, expr_type, UnOpNot, pretyped_expr) )
      (* TODO: Implement after Tuple *)
      | UnOpFst | UnOpSnd ->
          Ok
            ( typing_context,
              TyCustom (Type_name.of_string "_undefined"),
              [],
              Pretyped_ast.Unit
                (loc, TyCustom (Type_name.of_string "_undefined")) ))
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
  | FunApp (loc, function_name, function_params) ->
      let open Result in
      Functions_env.get_function_by_name loc function_name functions_env
      >>= fun (FunctionEnvEntry (_, function_args_types, function_return_type))
        ->
      if List.length function_args_types <> List.length function_params then
        Or_error.of_exn PartialFunctionApplicationNotAllowed
      else
        zip_lists function_args_types function_params
        >>| List.fold_left ~init:([], [])
              ~f:(fun
                  (acc_constraints, acc_pretyped_params)
                  (function_arg_type, function_param)
                ->
                Or_error.ok_exn
                  ( generate_constraints constructors_env functions_env
                      typing_context function_param ~verbose
                  >>= fun ( _,
                            function_param_type,
                            function_param_constraints,
                            pretyped_param ) ->
                    Ok
                      ( ( convert_ast_type_to_ty function_arg_type,
                          function_param_type )
                        :: function_param_constraints
                        @ acc_constraints,
                        acc_pretyped_params @ [ pretyped_param ] ) ))
        >>= fun (function_args_constraints, pretyped_args) ->
        Ok
          ( typing_context,
            convert_ast_type_to_ty function_return_type,
            function_args_constraints,
            Pretyped_ast.FunApp
              ( loc,
                convert_ast_type_to_ty function_return_type,
                function_name,
                pretyped_args ) )
  | Match (loc, var_name, pattern_exprs) ->
      let t = fresh () in
      Type_context_env.get_var_type typing_context var_name
      >>= fun matched_var_type ->
      Ok
        (List.fold_left pattern_exprs ~init:([], [])
           ~f:(fun
               (acc_constraints, acc_pretyped_pattern_exprs)
               (MPattern (loc, matched_expr, block_expr))
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env []
                   matched_expr ~verbose
               >>= fun ( matched_typing_context,
                         matched_expr_type,
                         match_expr_constraints,
                         pretyped_matched_expr ) ->
                 let union_typing_contexts =
                   Type_context_env.prepend_typing_contexts
                     matched_typing_context typing_context
                 in
                 generate_constraints_block_expr constructors_env functions_env
                   union_typing_contexts block_expr ~verbose
                 >>= fun ( _,
                           block_expr_type,
                           block_expr_constraints,
                           pretyped_block_expr ) ->
                 Ok
                   ( (matched_var_type, matched_expr_type)
                     :: (t, block_expr_type) :: match_expr_constraints
                     @ block_expr_constraints @ acc_constraints,
                     Pretyped_ast.MPattern
                       ( loc,
                         block_expr_type,
                         pretyped_matched_expr,
                         pretyped_block_expr )
                     :: acc_pretyped_pattern_exprs ) )))
      >>= fun (match_expr_constraints, pretyped_pattern_exprs) ->
      Ok
        ( typing_context,
          t,
          match_expr_constraints,
          Pretyped_ast.Match (loc, t, var_name, pretyped_pattern_exprs) )
  | DMatch (loc, var_name, pattern_exprs) ->
      let t = fresh () in
      Type_context_env.get_var_type typing_context var_name
      >>= fun matched_var_type ->
      Ok
        (List.fold_left pattern_exprs ~init:([], [])
           ~f:(fun
               (acc_constraints, acc_pretyped_pattern_exprs)
               (MPattern (loc, matched_expr, block_expr))
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env []
                   matched_expr ~verbose
               >>= fun ( matched_typing_context,
                         matched_expr_type,
                         match_expr_constraints,
                         pretyped_matched_expr ) ->
                 let union_typing_contexts =
                   Type_context_env.prepend_typing_contexts
                     matched_typing_context typing_context
                 in
                 generate_constraints_block_expr constructors_env functions_env
                   union_typing_contexts block_expr ~verbose
                 >>= fun ( _,
                           block_expr_type,
                           block_expr_constraints,
                           pretyped_block_expr ) ->
                 Ok
                   ( (matched_var_type, matched_expr_type)
                     :: (t, block_expr_type) :: match_expr_constraints
                     @ block_expr_constraints @ acc_constraints,
                     Pretyped_ast.MPattern
                       ( loc,
                         block_expr_type,
                         pretyped_matched_expr,
                         pretyped_block_expr )
                     :: acc_pretyped_pattern_exprs ) )))
      >>= fun (match_expr_constraints, pretyped_pattern_exprs) ->
      Ok
        ( typing_context,
          t,
          match_expr_constraints,
          Pretyped_ast.DMatch (loc, t, var_name, pretyped_pattern_exprs) ))
  >>= fun (typing_context, expr_ty, expr_constraints, pretyped_expr) ->
  Pprint_type_infer.pprint_type_infer_expr_verbose Fmt.stdout ~verbose expr
    typing_context expr_ty expr_constraints;
  Ok (typing_context, expr_ty, expr_constraints, pretyped_expr)

and generate_constraints_matched_expr
    (constructors_env : Type_defns_env.constructors_env)
    (matched_typing_context : typing_context) (matched_expr : matched_expr)
    ~(verbose : bool) :
    (typing_context * ty * constr list * Pretyped_ast.matched_expr) Or_error.t =
  match matched_expr with
  | MUnderscore loc ->
      let t = fresh () in
      Ok ([], t, [], Pretyped_ast.MUnderscore (loc, t))
  | MVariable (loc, matched_var_name) ->
      let t = fresh () in
      let open Result in
      let extended_typing_context =
        Type_context_env.extend_typing_context matched_typing_context
          matched_var_name t
      in
      Ok
        ( extended_typing_context,
          t,
          [],
          Pretyped_ast.MVariable (loc, t, matched_var_name) )
  | MTuple (loc, matched_expr_fst, matched_expr_snd) ->
      let t = fresh () in
      let open Result in
      generate_constraints_matched_expr constructors_env matched_typing_context
        matched_expr_fst ~verbose
      >>= fun ( matched_typing_context_fst,
                matched_expr_fst_type,
                matched_expr_fst_contraints,
                pretyped_matched_expr_fst ) ->
      generate_constraints_matched_expr constructors_env matched_typing_context
        matched_expr_snd ~verbose
      >>= fun ( matched_typing_context_snd,
                matched_expr_snd_type,
                matched_expr_snd_contraints,
                pretyped_matched_expr_snd ) ->
      let union_matched_typing_context =
        Type_context_env.prepend_typing_contexts matched_typing_context_fst
          matched_typing_context_snd
      in
      Ok
        ( union_matched_typing_context,
          t,
          (t, TyTuple (matched_expr_fst_type, matched_expr_snd_type))
          :: matched_expr_fst_contraints
          @ matched_expr_snd_contraints,
          Pretyped_ast.MTuple
            (loc, t, pretyped_matched_expr_fst, pretyped_matched_expr_snd) )
  | MConstructor (loc, constructor_name, matched_exprs) ->
      let open Result in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (Type_defns_env.ConstructorEnvEntry
                (constructor_type_name, _, constructor_arg_types)) ->
      zip_lists matched_exprs constructor_arg_types
      >>= fun matched_expr_types ->
      Ok
        (List.fold_left matched_expr_types
           ~init:(matched_typing_context, [], [])
           ~f:(fun
               ( acc_matched_typing_context,
                 acc_matched_expr_constraints,
                 acc_pretyped_matched_expr )
               (matched_expr, matched_expr_type)
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env
                   acc_matched_typing_context matched_expr ~verbose
               >>= fun ( matched_typing_context,
                         matched_expr_ty,
                         matched_expr_constraints,
                         pretyped_matched_expr ) ->
                 let union_matched_typing_context =
                   Type_context_env.prepend_typing_contexts
                     acc_matched_typing_context matched_typing_context
                 in
                 Ok
                   ( union_matched_typing_context,
                     (matched_expr_ty, convert_ast_type_to_ty matched_expr_type)
                     :: matched_expr_constraints
                     @ acc_matched_expr_constraints,
                     pretyped_matched_expr :: acc_pretyped_matched_expr ) )))
      >>= fun ( matched_typing_context,
                matched_expr_constraints,
                pretyped_matched_exprs ) ->
      Ok
        ( matched_typing_context,
          TyCustom constructor_type_name,
          matched_expr_constraints,
          Pretyped_ast.MConstructor
            ( loc,
              TyCustom constructor_type_name,
              constructor_name,
              pretyped_matched_exprs ) )
  | MOption (loc, matched_expr) -> (
      let t = fresh () in
      match matched_expr with
      | None ->
          Ok (matched_typing_context, t, [], Pretyped_ast.MOption (loc, t, None))
      | Some matched_expr ->
          let open Result in
          generate_constraints_matched_expr constructors_env
            matched_typing_context matched_expr ~verbose
          >>= fun ( matched_typing_context,
                    match_expr_type,
                    match_expr_constraints,
                    pretyped_matched_expr ) ->
          Ok
            ( matched_typing_context,
              t,
              (t, TyOption match_expr_type) :: match_expr_constraints,
              Pretyped_ast.MOption (loc, t, Some pretyped_matched_expr) ))
