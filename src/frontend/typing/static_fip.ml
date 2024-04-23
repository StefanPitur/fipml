open Ast.Ast_types
open Borrowed_context
open Core
open Owned_context
open Parsing
open Reuse_credits
open Result

exception UnableFipCheck
exception TailContextFailure

let check_bounded_stack_allocation (function_mut_rec_id : int)
    (function_body : Typed_ast.expr)
    (functions_env : Functions_env.functions_env) : unit Or_error.t =
  let mutually_recursive_functions_var_names : Parser_ast.FreeVarSet.t =
    List.fold
      (Functions_env.get_mutually_recursive_functions_env_by_group_id
         function_mut_rec_id functions_env) ~init:Parser_ast.FreeVarSet.empty
      ~f:(fun
          acc_free_var_set
          (Functions_env.FunctionEnvEntry (_, _, fun_env_entry_name, _, _))
        ->
        Set.add acc_free_var_set
          (Var_name.of_string (Function_name.to_string fun_env_entry_name)))
  in

  let f_bar_and_fv_are_disjoint (fv : Parser_ast.FreeVarSet.t) : unit Or_error.t
      =
    if Set.are_disjoint mutually_recursive_functions_var_names fv then Ok ()
    else Or_error.of_exn TailContextFailure
  in

  let rec check_tail_context_calculus (expr : Typed_ast.expr) : unit Or_error.t
      =
    match expr with
    | Let (_, _, _, var_expr, expr) ->
        f_bar_and_fv_are_disjoint
          (Parser_ast.free_variables
             (Typed_ast.convert_typed_to_parser var_expr))
        >>= fun _ -> check_tail_context_calculus expr
    | FunCall (loc, _, function_name, function_args)
      when Or_error.ok_exn
             (Functions_env.get_function_mutually_recursive_group_id loc
                function_name functions_env)
           = function_mut_rec_id ->
        f_bar_and_fv_are_disjoint
          (Parser_ast.free_variables_values
             (Typed_ast.convert_typed_to_parser_values function_args))
    | If (_, _, cond_expr, then_expr) ->
        f_bar_and_fv_are_disjoint
          (Parser_ast.free_variables
             (Typed_ast.convert_typed_to_parser cond_expr))
        >>= fun _ -> check_tail_context_calculus then_expr
    | IfElse (_, _, cond_expr, then_expr, else_expr) ->
        f_bar_and_fv_are_disjoint
          (Parser_ast.free_variables
             (Typed_ast.convert_typed_to_parser cond_expr))
        >>= fun _ ->
        check_tail_context_calculus then_expr >>= fun _ ->
        check_tail_context_calculus else_expr
    | Match (_, _, _, _, pattern_exprs) ->
        Ok
          (List.iter pattern_exprs
             ~f:(fun (Typed_ast.MPattern (_, _, _, expr)) ->
               Or_error.ok_exn (check_tail_context_calculus expr)))
    | Drop (_, _, _, _, expr)
    | Free (_, _, _, expr)
    | Weak (_, _, _, expr)
    | Inst (_, _, _, expr) ->
        check_tail_context_calculus expr
    | _ ->
        f_bar_and_fv_are_disjoint
          (Parser_ast.free_variables (Typed_ast.convert_typed_to_parser expr))
  in
  check_tail_context_calculus function_body

let fip
    (TFun (_, _, group_id, fip_option, _, params, function_body) :
      Typed_ast.function_defn) (functions_env : Functions_env.functions_env) :
    Fip_ast.expr Or_error.t =
  match fip_option with
  | Some (Fip n) ->
      check_bounded_stack_allocation group_id function_body functions_env
      >>= fun _ ->
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
      Fip_rules_check.fip_rules_check_expr function_body borrowed_set
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
        Ok fip_function_body
      else Or_error.of_exn UnableFipCheck
  | _ -> Or_error.of_exn UnableFipCheck
