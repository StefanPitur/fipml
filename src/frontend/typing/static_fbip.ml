open Ast.Ast_types
open Borrowed_context
open Core
open Owned_context
open Reuse_credits
open Result

exception UnableFbipCheck

let fbip
    (TFun (loc, _, _, fip_option, function_name, params, function_body) :
      Typed_ast.function_defn) (functions_env : Functions_env.functions_env) :
    Fip_ast.function_defn Or_error.t =
  Fmt.pf Fmt.stdout "Find me - %s@." (Function_name.to_string function_name);
  match fip_option with
  | Some (Fbip n as fbip) ->
      let borrowed_set, owned_set =
        List.fold params ~init:(BorrowedSet.empty, OwnedSet.empty)
          ~f:(fun
              (acc_borrowed_set, acc_owned_set)
              (TParam (param_type_expr, param_name, param_borrowed_option))
            ->
            match param_borrowed_option with
            | None ->
                let extended_owned_set =
                  Or_error.ok_exn
                    (extend_owned_set ~element:param_name
                       ~element_type_expr:param_type_expr
                       ~owned_set:acc_owned_set)
                in
                (acc_borrowed_set, extended_owned_set)
            | Some _ ->
                let extended_borrowed_set =
                  Or_error.ok_exn
                    (extend_borrowed_set ~element:param_name
                       ~borrowed_set:acc_borrowed_set)
                in
                (extended_borrowed_set, acc_owned_set))
      in
      let reuse_map =
        extend_reuse_map_k_times
          ~reuse_size:(allocation_credit_size ())
          ~reuse_var:(Var_name.of_string "_new")
          ~k:n ~reuse_map:ReuseMap.empty
      in
      Fbip_rules_check.fbip_rules_check_expr function_body borrowed_set
        functions_env
      >>= fun fip_function_body ->
      let _, fip_owned_set, fip_reuse_map =
        Fip_ast.get_fip_contexts_from_expr fip_function_body
      in
      let owned_set_restriction = Set.equal fip_owned_set owned_set in
      let reuse_map_restriction =
        Map.equal reuse_map_entry_equal_fn fip_reuse_map reuse_map
      in
      if owned_set_restriction && reuse_map_restriction then
        let fbiped_function_name =
          Function_name.of_string (Function_name.to_string function_name ^ "!")
        in
        let fbiped_params = Fip_ast.convert_params_to_fip_params params in
        Ok
          (Fip_ast.TFun
             (loc, fbip, fbiped_function_name, fbiped_params, fip_function_body))
      else Or_error.of_exn UnableFbipCheck
  | _ -> Or_error.of_exn UnableFbipCheck
