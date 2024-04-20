open Borrowed_context
open Core
open Owned_context
open Typing
open Reuse_credits
open Result

exception InstructionNotAllowedInFipFunction of string

let rec fip_rules_check_value (typed_value : Typed_ast.value)
    (borrowed_set : BorrowedSet.t) : Fip_ast.value Or_error.t =
  match typed_value with
  | Unit (loc, _) ->
      Ok (Unit (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty))
  | Integer (loc, _, i) ->
      Ok (Integer (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty, i))
  | Boolean (loc, _, b) ->
      Ok (Boolean (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty, b))
  | Variable (loc, _, var_name) ->
      let owned_set = OwnedSet.singleton var_name in
      Ok (Variable (loc, borrowed_set, owned_set, ReuseMap.empty, var_name))
  | Constructor (loc, _, constructor_name, constructor_typed_values) ->
      let constructor_arity = List.length constructor_typed_values in
      if constructor_arity = 0 then
        Ok
          (Constructor
             ( loc,
               borrowed_set,
               OwnedSet.empty,
               ReuseMap.empty,
               constructor_name,
               [] ))
      else
        let owned_set, reuse_map, constructor_fip_values =
          List.fold_right constructor_typed_values
            ~init:(OwnedSet.empty, ReuseMap.empty, [])
            ~f:(fun
                constructor_typed_value
                (acc_owned_set, acc_reuse_map, acc_fip_values)
              ->
              Or_error.ok_exn
                ( fip_rules_check_value constructor_typed_value borrowed_set
                >>= fun fip_value ->
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
                      fip_value :: acc_fip_values ) ))
        in
        let extended_reuse_map =
          extend_reuse_map ~reuse_size:constructor_arity
            ~reuse_var:(Ast.Ast_types.Var_name.of_string "_todo")
            ~reuse_map
        in
        Ok
          (Constructor
             ( loc,
               borrowed_set,
               owned_set,
               extended_reuse_map,
               constructor_name,
               constructor_fip_values ))

and fip_rules_check_expr (typed_expr : Typed_ast.expr)
    (borrowed_set : BorrowedSet.t) (functions_env : Functions_env.functions_env)
    : Fip_ast.expr Or_error.t =
  match typed_expr with
  | UnboxedSingleton (loc, _, typed_value) ->
      fip_rules_check_value typed_value borrowed_set >>= fun fip_value ->
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_value fip_value
      in
      Ok
        (Fip_ast.UnboxedSingleton
           (loc, borrowed_set, owned_set, reuse_map, fip_value))
  | UnboxedTuple (loc, _, typed_values) ->
      let owned_set, reuse_map, fip_values =
        List.fold_right typed_values ~init:(OwnedSet.empty, ReuseMap.empty, [])
          ~f:(fun typed_value (acc_owned_set, acc_reuse_map, acc_fip_values) ->
            Or_error.ok_exn
              ( fip_rules_check_value typed_value borrowed_set
              >>= fun fip_value ->
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
                    fip_value :: acc_fip_values ) ))
      in
      Ok
        (Fip_ast.UnboxedTuple
           (loc, borrowed_set, owned_set, reuse_map, fip_values))
  | Let (loc, _, var_names, var_expr, expr) ->
      fip_rules_check_expr expr borrowed_set functions_env >>= fun fip_expr ->
      let _, expr_extended_owned_set, expr_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let expr_owned_set =
        Or_error.ok_exn
          (remove_elements_from_owned_set ~elements:var_names
             ~owned_set:expr_extended_owned_set)
      in
      let var_expr_borrowed_set =
        combine_owned_with_borrowed ~borrowed_set ~owned_set:expr_owned_set
      in
      fip_rules_check_expr var_expr var_expr_borrowed_set functions_env
      >>= fun fip_var_expr ->
      let _, var_expr_owned_set, var_expr_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_var_expr
      in
      let combined_reuse_map =
        combine_reuse_maps ~reuse_map1:expr_reuse_map
          ~reuse_map2:var_expr_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          (combine_owned_sets ~owned_set1:expr_owned_set
             ~owned_set2:var_expr_owned_set)
      in
      Ok
        (Fip_ast.Let
           ( loc,
             borrowed_set,
             combined_owned_set,
             combined_reuse_map,
             var_names,
             fip_var_expr,
             fip_expr ))
  | FunApp (loc, _, fun_var, values) ->
      Or_error.ok_exn (assert_in_borrowed_set ~element:fun_var ~borrowed_set);
      let owned_set, reuse_map, fip_values =
        List.fold_right values ~init:(OwnedSet.empty, ReuseMap.empty, [])
          ~f:(fun value (acc_owned_set, acc_reuse_map, acc_fip_values) ->
            Or_error.ok_exn
              ( fip_rules_check_value value borrowed_set >>= fun fip_value ->
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
                    fip_value :: acc_fip_values ) ))
      in
      Ok
        (Fip_ast.FunApp
           (loc, borrowed_set, owned_set, reuse_map, fun_var, fip_values))
  | FunCall (loc, _, function_name, values) ->
      Or_error.ok_exn
        (Functions_env.assert_function_has_required_fip_type loc (Fip (-1))
           function_name functions_env);
      let function_allocation_credit =
        Or_error.ok_exn
          (Functions_env.get_fip_function_allocation_credit loc function_name
             functions_env)
      in
      let owned_set, reuse_map, fip_values, values_acc_allocation_credit =
        List.fold_right values ~init:(OwnedSet.empty, ReuseMap.empty, [], 0)
          ~f:(fun
              value
              ( acc_owned_set,
                acc_reuse_map,
                acc_fip_values,
                acc_allocation_credit )
            ->
            match
              Fip_ast.is_value_borrowed_or_top_level_fip_function loc ~value
                ~required_fip_type:(Fip (-1)) ~borrowed_set ~functions_env
            with
            | Ok (var_name, allocation_credit) ->
                let fip_value =
                  Fip_ast.Variable
                    (loc, borrowed_set, OwnedSet.empty, ReuseMap.empty, var_name)
                in
                ( acc_owned_set,
                  acc_reuse_map,
                  fip_value :: acc_fip_values,
                  allocation_credit + acc_allocation_credit )
            | _ ->
                Or_error.ok_exn
                  ( fip_rules_check_value value borrowed_set >>= fun fip_value ->
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
                        acc_allocation_credit ) ))
      in
      let extended_reuse_map =
        extend_reuse_map_k_times
          ~reuse_size:(allocation_credit_size ())
          ~reuse_var:(Ast.Ast_types.Var_name.of_string "_new")
          ~k:(values_acc_allocation_credit + function_allocation_credit)
          ~reuse_map
      in
      Ok
        (Fip_ast.FunCall
           ( loc,
             borrowed_set,
             owned_set,
             extended_reuse_map,
             function_name,
             fip_values ))
  | If (loc, _, cond_expr, then_expr) ->
      fip_rules_check_expr cond_expr borrowed_set functions_env
      >>= fun fip_cond_expr ->
      let _, cond_owned_set, cond_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_cond_expr
      in
      fip_rules_check_expr then_expr borrowed_set functions_env
      >>= fun fip_then_expr ->
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
      Ok
        (Fip_ast.If
           ( loc,
             borrowed_set,
             combined_owned_set,
             combined_reuse_map,
             fip_cond_expr,
             fip_then_expr ))
  | IfElse (loc, _, cond_expr, then_expr, else_expr) ->
      fip_rules_check_expr cond_expr borrowed_set functions_env
      >>= fun fip_cond_expr ->
      let _, cond_owned_set, cond_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_cond_expr
      in

      fip_rules_check_expr then_expr borrowed_set functions_env
      >>= fun fip_then_expr ->
      let _, then_owned_set, then_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_then_expr
      in

      fip_rules_check_expr else_expr borrowed_set functions_env
      >>= fun fip_else_expr ->
      let _, else_owned_set, else_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_else_expr
      in
      Or_error.ok_exn
        (assert_owned_sets_are_equal ~owned_set1:then_owned_set
           ~owned_set2:else_owned_set);
      Or_error.ok_exn
        (assert_reuse_maps_are_equal ~reuse_map1:then_reuse_map
           ~reuse_map2:else_reuse_map);
      let combined_reuse_map =
        combine_reuse_maps ~reuse_map1:cond_reuse_map ~reuse_map2:then_reuse_map
      in
      let combined_owned_set =
        Or_error.ok_exn
          (combine_owned_sets ~owned_set1:cond_owned_set
             ~owned_set2:then_owned_set)
      in
      Ok
        (Fip_ast.IfElse
           ( loc,
             borrowed_set,
             combined_owned_set,
             combined_reuse_map,
             fip_cond_expr,
             fip_then_expr,
             fip_else_expr ))
  | Match (loc, _, matched_var, pattern_exprs) -> (
      match assert_in_borrowed_set ~element:matched_var ~borrowed_set with
      | Ok () -> (
          let fip_pattern_exprs =
            List.map pattern_exprs
              ~f:(fun (Typed_ast.MPattern (loc, _, matched_expr, typed_expr)) ->
                Or_error.ok_exn
                  (let matched_expr_vars =
                     Typed_ast.get_matched_expr_vars matched_expr
                   in
                   extend_borrowed_set_by_list ~elements:matched_expr_vars
                     ~borrowed_set
                   >>= fun extended_borrowed_set ->
                   fip_rules_check_expr typed_expr extended_borrowed_set
                     functions_env
                   >>= fun fip_expr ->
                   let _, owned_set, reuse_map =
                     Fip_ast.get_fip_contexts_from_expr fip_expr
                   in
                   assert_elements_not_in_owned_set ~elements:matched_expr_vars
                     ~owned_set
                   >>= fun () ->
                   Ok
                     (Fip_ast.MPattern
                        ( loc,
                          borrowed_set,
                          owned_set,
                          reuse_map,
                          matched_expr,
                          fip_expr ))))
          in
          let fip_pattern_exprs_owned_sets =
            List.map fip_pattern_exprs ~f:(fun fip_pattern_expr ->
                let _, o, _ =
                  Fip_ast.get_fip_contexts_from_pattern_expr fip_pattern_expr
                in
                o)
          in
          let fip_pattern_exprs_reuse_maps =
            List.map fip_pattern_exprs ~f:(fun fip_pattern_expr ->
                let _, _, r =
                  Fip_ast.get_fip_contexts_from_pattern_expr fip_pattern_expr
                in
                r)
          in
          let fip_all_equal_owned_set =
            List.all_equal fip_pattern_exprs_owned_sets ~equal:OwnedSet.equal
          in
          let fip_all_equal_reuse_map =
            List.all_equal fip_pattern_exprs_reuse_maps
              ~equal:reuse_map_equal_fn
          in
          match (fip_all_equal_owned_set, fip_all_equal_reuse_map) with
          | Some fip_owned_set, Some fip_reuse_map ->
              Ok
                (Match
                   ( loc,
                     borrowed_set,
                     fip_owned_set,
                     fip_reuse_map,
                     matched_var,
                     fip_pattern_exprs ))
          | _ ->
              raise
                (Invalid_argument
                   (Fmt.str
                      "In a borrowed match, owned contexts should be equal on \
                       all branches - %s."
                      (Ast.Ast_types.string_of_loc loc))))
      | _ -> (
          let fip_pattern_exprs =
            List.map pattern_exprs
              ~f:(fun (Typed_ast.MPattern (loc, _, matched_expr, typed_expr)) ->
                Or_error.ok_exn
                  (let matched_expr_vars =
                     Typed_ast.get_matched_expr_vars matched_expr
                   in
                   let match_expr_reuse_credits =
                     Typed_ast.get_match_expr_reuse_credits matched_expr
                   in
                   assert_elements_not_in_borrowed_set
                     ~elements:matched_expr_vars ~borrowed_set
                   >>= fun () ->
                   fip_rules_check_expr typed_expr borrowed_set functions_env
                   >>= fun fip_expr ->
                   let _, expr_owned_set, expr_reuse_map =
                     Fip_ast.get_fip_contexts_from_expr fip_expr
                   in
                   remove_elements_from_owned_set ~elements:matched_expr_vars
                     ~owned_set:expr_owned_set
                   >>= fun owned_set ->
                   let reuse_map =
                     List.fold match_expr_reuse_credits ~init:expr_reuse_map
                       ~f:(fun expr_reuse_map match_expr_reuse_credit ->
                         Or_error.ok_exn
                           (consume_reuse_map
                              ~reuse_size:match_expr_reuse_credit
                              ~reuse_map:expr_reuse_map))
                   in
                   Ok
                     (Fip_ast.MPattern
                        ( loc,
                          borrowed_set,
                          owned_set,
                          reuse_map,
                          matched_expr,
                          fip_expr ))))
          in
          let fip_pattern_exprs_owned_sets =
            List.map fip_pattern_exprs ~f:(fun fip_pattern_expr ->
                let _, o, _ =
                  Fip_ast.get_fip_contexts_from_pattern_expr fip_pattern_expr
                in
                o)
          in

          let fip_pattern_exprs_reuse_maps =
            List.map fip_pattern_exprs ~f:(fun fip_pattern_expr ->
                let _, _, r =
                  Fip_ast.get_fip_contexts_from_pattern_expr fip_pattern_expr
                in
                r)
          in
          let fip_all_equal_owned_set =
            List.all_equal fip_pattern_exprs_owned_sets ~equal:OwnedSet.equal
          in
          let fip_all_equal_reuse_map =
            List.all_equal fip_pattern_exprs_reuse_maps
              ~equal:reuse_map_equal_fn
          in
          match (fip_all_equal_owned_set, fip_all_equal_reuse_map) with
          | Some fip_owned_set, Some fip_reuse_map ->
              let extended_owned_set =
                Or_error.ok_exn
                  (extend_owned_set ~element:matched_var
                     ~owned_set:fip_owned_set)
              in
              Ok
                (DMatch
                   ( loc,
                     borrowed_set,
                     extended_owned_set,
                     fip_reuse_map,
                     matched_var,
                     fip_pattern_exprs ))
          | _ ->
              raise
                (Invalid_argument
                   (Fmt.str
                      "In a borrowed match, owned contexts should be equal on \
                       all branches - %s."
                      (Ast.Ast_types.string_of_loc loc)))))
  | UnOp (loc, _, unary_op, expr) ->
      fip_rules_check_expr expr borrowed_set functions_env >>= fun fip_expr ->
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      Ok
        (Fip_ast.UnOp
           (loc, borrowed_set, owned_set, reuse_map, unary_op, fip_expr))
  | BinaryOp (loc, _, binary_op, left_expr, right_expr) ->
      fip_rules_check_expr left_expr borrowed_set functions_env
      >>= fun fip_left_expr ->
      let _, left_owned_set, left_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_left_expr
      in
      fip_rules_check_expr right_expr borrowed_set functions_env
      >>= fun fip_right_expr ->
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
      Ok
        (Fip_ast.BinaryOp
           ( loc,
             borrowed_set,
             combined_owned_set,
             combined_reuse_map,
             binary_op,
             fip_left_expr,
             fip_right_expr ))
  | Drop (loc, _, _, _) ->
      let error_string = "free - " ^ Ast.Ast_types.string_of_loc loc in
      Or_error.of_exn (InstructionNotAllowedInFipFunction error_string)
  | Free (loc, _, k, expr) ->
      fip_rules_check_expr expr borrowed_set functions_env >>= fun fip_expr ->
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let extended_reuse_map =
        extend_reuse_map ~reuse_size:k
          ~reuse_var:(Ast.Ast_types.Var_name.of_string "_free")
          ~reuse_map
      in
      Ok
        (Fip_ast.Free
           (loc, borrowed_set, owned_set, extended_reuse_map, k, fip_expr))
  | Weak (loc, _, k, expr) ->
      fip_rules_check_expr expr borrowed_set functions_env >>= fun fip_expr ->
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let extended_reuse_map =
        extend_reuse_map_k_times
          ~reuse_size:(allocation_credit_size ())
          ~reuse_var:(Ast.Ast_types.Var_name.of_string "_new")
          ~k ~reuse_map
      in
      Ok
        (Fip_ast.Weak
           (loc, borrowed_set, owned_set, extended_reuse_map, k, fip_expr))
  | Inst (loc, _, k, expr) ->
      fip_rules_check_expr expr borrowed_set functions_env >>= fun fip_expr ->
      let _, owned_set, reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_expr
      in
      let shrinked_reuse_map =
        Or_error.ok_exn (consume_reuse_map ~reuse_size:k ~reuse_map)
      in
      let update_reuse_map =
        extend_reuse_map
          ~reuse_size:(allocation_credit_size ())
          ~reuse_var:(Ast.Ast_types.Var_name.of_string "_new")
          ~reuse_map:shrinked_reuse_map
      in
      Ok
        (Fip_ast.Inst
           (loc, borrowed_set, owned_set, update_reuse_map, k, fip_expr))
