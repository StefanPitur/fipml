open Borrowed_context
open Core
open Owned_context
open Typing
open Reuse_credits
open Result

exception NonOwnedVariableNotFound

let rec fip_rules_check_value (typed_value : Typed_ast.value)
    (borrowed_set : BorrowedSet.t) : Fip_ast.value =
  match typed_value with
  | Unit (loc, _) -> Unit (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty)
  | Integer (loc, _, i) ->
      Integer (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty, i)
  | Boolean (loc, _, b) ->
      Boolean (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty, b)
  | Variable (loc, _, var_name) ->
      let owned_set = OwnedSet.singleton var_name in
      Variable (loc, borrowed_set, owned_set, ReuseMap.empty, var_name)
  | Constructor (loc, _, constructor_name, constructor_typed_values) ->
      let constructor_arity = List.length constructor_typed_values in
      if constructor_arity = 0 then
        Constructor
          ( loc,
            borrowed_set,
            OwnedSet.empty,
            ReuseMap.empty,
            constructor_name,
            [] )
      else
        let owned_set, reuse_map, constructor_fip_values =
          List.fold_right constructor_typed_values
            ~init:(OwnedSet.empty, ReuseMap.empty, [])
            ~f:(fun
                constructor_typed_value
                (acc_owned_set, acc_reuse_map, acc_fip_values)
              ->
              Or_error.ok_exn
                (let fip_value =
                   fip_rules_check_value constructor_typed_value borrowed_set
                 in
                 let _, owned_set, reuse_map =
                   Fip_ast.get_fip_contexts_from_value fip_value
                 in
                 let combined_reuse_map =
                   combine_reuse_maps ~reuse_map1:acc_reuse_map
                     ~reuse_map2:reuse_map
                 in
                 combine_owned_sets ~owned_set1:acc_owned_set
                   ~owned_set2:owned_set
                 >>= fun combined_owned_set ->
                 Ok
                   ( combined_owned_set,
                     combined_reuse_map,
                     fip_value :: acc_fip_values )))
        in
        let extended_reuse_map =
          extend_reuse_map ~reuse_size:constructor_arity
            ~reuse_var:(Ast.Ast_types.Var_name.of_string "_todo")
            ~reuse_map
        in
        Constructor
          ( loc,
            borrowed_set,
            owned_set,
            extended_reuse_map,
            constructor_name,
            constructor_fip_values )

and fip_rules_check_expr (typed_expr : Typed_ast.expr)
    (borrowed_set : BorrowedSet.t) (functions_env : Functions_env.functions_env)
    : Fip_ast.expr =
  match typed_expr with
  | UnboxedSingleton (loc, _, typed_value) ->
      let fip_value = fip_rules_check_value typed_value borrowed_set in
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_value fip_value
      in
      UnboxedSingleton (loc, borrowed_set, owned_set, reuse_map, fip_value)
  | UnboxedTuple (loc, _, typed_values) ->
      let owned_set, reuse_map, fip_values =
        List.fold_right typed_values ~init:(OwnedSet.empty, ReuseMap.empty, [])
          ~f:(fun typed_value (acc_owned_set, acc_reuse_map, acc_fip_values) ->
            Or_error.ok_exn
              (let fip_value = fip_rules_check_value typed_value borrowed_set in
               let _, owned_set, reuse_map =
                 Fip_ast.get_fip_contexts_from_value fip_value
               in
               let combined_reuse_map =
                 combine_reuse_maps ~reuse_map1:acc_reuse_map
                   ~reuse_map2:reuse_map
               in
               combine_owned_sets ~owned_set1:acc_owned_set
                 ~owned_set2:owned_set
               >>= fun combined_owned_set ->
               Ok
                 ( combined_owned_set,
                   combined_reuse_map,
                   fip_value :: acc_fip_values )))
      in
      UnboxedTuple (loc, borrowed_set, owned_set, reuse_map, fip_values)
  | Let (loc, _, var_names, var_expr, expr) ->
      let fip_expr = fip_rules_check_expr expr borrowed_set functions_env in
      let _, expr_owned_set, expr_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let gamma_2 =
        Or_error.ok_exn
          (remove_elements_from_owned_set ~elements:var_names
             ~owned_set:expr_owned_set)
      in
      let var_expr_borrowed_set =
        combine_owned_with_borrowed ~borrowed_set ~owned_set:gamma_2
      in
      let fip_var_expr =
        fip_rules_check_expr var_expr var_expr_borrowed_set functions_env
      in
      let _, var_expr_owned_set, var_expr_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_var_expr
      in
      let combined_reuse_map =
        combine_reuse_maps ~reuse_map1:expr_reuse_map
          ~reuse_map2:var_expr_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          (combine_owned_sets ~owned_set1:gamma_2 ~owned_set2:var_expr_owned_set)
      in
      Let
        ( loc,
          borrowed_set,
          combined_owned_set,
          combined_reuse_map,
          var_names,
          fip_var_expr,
          fip_expr )
  | FunApp (loc, _, fun_var, values) ->
      Or_error.ok_exn (assert_in_borrowed_set ~element:fun_var ~borrowed_set);
      let owned_set, reuse_map, fip_values =
        List.fold_right values ~init:(OwnedSet.empty, ReuseMap.empty, [])
          ~f:(fun value (acc_owned_set, acc_reuse_map, acc_fip_values) ->
            Or_error.ok_exn
              (let fip_value = fip_rules_check_value value borrowed_set in
               let _, owned_set, reuse_map =
                 Fip_ast.get_fip_contexts_from_value fip_value
               in
               let combined_reuse_map =
                 combine_reuse_maps ~reuse_map1:acc_reuse_map
                   ~reuse_map2:reuse_map
               in
               combine_owned_sets ~owned_set1:acc_owned_set
                 ~owned_set2:owned_set
               >>= fun combined_owned_set ->
               Ok
                 ( combined_owned_set,
                   combined_reuse_map,
                   fip_value :: acc_fip_values )))
      in
      FunApp (loc, borrowed_set, owned_set, reuse_map, fun_var, fip_values)
  | FunCall (loc, _, function_name, values) ->
      let owned_set, reuse_map, fip_values, values_acc_allocation_credit =
        List.fold_right values ~init:(OwnedSet.empty, ReuseMap.empty, [], 0)
          ~f:(fun
              value
              ( acc_owned_set,
                acc_reuse_map,
                acc_fip_values,
                acc_allocation_credit )
            ->
            let ( var_name_allocation_credit_option,
                  is_value_borrowed_or_top_level_function ) =
              Fip_ast.is_value_borrowed_or_top_level_function loc ~value
                ~borrowed_set ~functions_env
            in
            if is_value_borrowed_or_top_level_function then
              match var_name_allocation_credit_option with
              | None -> raise NonOwnedVariableNotFound
              | Some (var_name, allocation_credit) ->
                  let fip_value =
                    Fip_ast.Variable
                      ( loc,
                        borrowed_set,
                        OwnedSet.empty,
                        ReuseMap.empty,
                        var_name )
                  in
                  ( acc_owned_set,
                    acc_reuse_map,
                    fip_value :: acc_fip_values,
                    allocation_credit + acc_allocation_credit )
            else
              Or_error.ok_exn
                (let fip_value = fip_rules_check_value value borrowed_set in
                 let _, owned_set, reuse_map =
                   Fip_ast.get_fip_contexts_from_value fip_value
                 in
                 let combined_reuse_map =
                   combine_reuse_maps ~reuse_map1:acc_reuse_map
                     ~reuse_map2:reuse_map
                 in
                 combine_owned_sets ~owned_set1:acc_owned_set
                   ~owned_set2:owned_set
                 >>= fun combined_owned_set ->
                 Ok
                   ( combined_owned_set,
                     combined_reuse_map,
                     fip_value :: acc_fip_values,
                     acc_allocation_credit )))
      in
      let function_allocation_credit =
        Functions_env.get_function_allocation_credit loc function_name
          functions_env
      in
      let extended_reuse_map =
        extend_reuse_map_k_times
          ~reuse_size:(allocation_credit_size ())
          ~reuse_var:(Ast.Ast_types.Var_name.of_string "fip")
          ~k:(values_acc_allocation_credit + function_allocation_credit)
          ~reuse_map
      in
      FunCall
        ( loc,
          borrowed_set,
          owned_set,
          extended_reuse_map,
          function_name,
          fip_values )
  | If (loc, _, cond_expr, then_expr) ->
      let fip_cond_expr =
        fip_rules_check_expr cond_expr borrowed_set functions_env
      in
      let _, cond_owned_set, cond_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_cond_expr
      in
      let fip_then_expr =
        fip_rules_check_expr then_expr borrowed_set functions_env
      in
      let _, then_owned_set, then_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_then_expr
      in
      let combined_reuse_map =
        combine_reuse_maps ~reuse_map1:cond_reuse_map ~reuse_map2:then_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          (combine_owned_sets ~owned_set1:cond_owned_set
             ~owned_set2:then_owned_set)
      in
      If
        ( loc,
          borrowed_set,
          combined_owned_set,
          combined_reuse_map,
          fip_cond_expr,
          fip_then_expr )
  | IfElse (loc, _, cond_expr, then_expr, else_expr) ->
      let fip_cond_expr =
        fip_rules_check_expr cond_expr borrowed_set functions_env
      in
      let _, cond_owned_set, cond_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_cond_expr
      in

      let fip_then_expr =
        fip_rules_check_expr then_expr borrowed_set functions_env
      in
      let _, then_owned_set, then_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_then_expr
      in

      let fip_else_expr =
        fip_rules_check_expr else_expr borrowed_set functions_env
      in
      let _, else_owned_set, else_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_else_expr
      in

      let combined_reuse_map =
        combine_reuse_maps
          ~reuse_map1:
            (combine_reuse_maps ~reuse_map1:cond_reuse_map
               ~reuse_map2:then_reuse_map)
          ~reuse_map2:else_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          ( combine_owned_sets ~owned_set1:cond_owned_set
              ~owned_set2:then_owned_set
          >>= fun cond_then_owned_set ->
            combine_owned_sets ~owned_set1:cond_then_owned_set
              ~owned_set2:else_owned_set )
      in
      IfElse
        ( loc,
          borrowed_set,
          combined_owned_set,
          combined_reuse_map,
          fip_cond_expr,
          fip_then_expr,
          fip_else_expr )
  | UnOp (loc, _, unary_op, expr) ->
      let fip_expr = fip_rules_check_expr expr borrowed_set functions_env in
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      UnOp (loc, borrowed_set, owned_set, reuse_map, unary_op, fip_expr)
  | BinaryOp (loc, _, binary_op, left_expr, right_expr) ->
      let fip_left_expr =
        fip_rules_check_expr left_expr borrowed_set functions_env
      in
      let _, left_owned_set, left_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_left_expr
      in
      let fip_right_expr =
        fip_rules_check_expr right_expr borrowed_set functions_env
      in
      let _, right_owned_set, right_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_right_expr
      in
      let combined_reuse_map =
        combine_reuse_maps ~reuse_map1:left_reuse_map
          ~reuse_map2:right_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          (combine_owned_sets ~owned_set1:left_owned_set
             ~owned_set2:right_owned_set)
      in
      BinaryOp
        ( loc,
          borrowed_set,
          combined_owned_set,
          combined_reuse_map,
          binary_op,
          fip_left_expr,
          fip_right_expr )
  | Drop (loc, _, drop_var, expr) ->
      let fip_expr = fip_rules_check_expr expr borrowed_set functions_env in
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let extended_owned_set =
        Or_error.ok_exn (extend_owned_set ~element:drop_var ~owned_set)
      in
      Drop (loc, borrowed_set, extended_owned_set, reuse_map, drop_var, fip_expr)
  | Free (loc, _, k, expr) ->
      let fip_expr = fip_rules_check_expr expr borrowed_set functions_env in
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let shrinked_reuse_map =
        Or_error.ok_exn (consume_reuse_map ~reuse_size:k ~reuse_map)
      in
      Free (loc, borrowed_set, owned_set, shrinked_reuse_map, k, fip_expr)
  | _ -> raise (Invalid_argument "Match fip rules not implemented")
