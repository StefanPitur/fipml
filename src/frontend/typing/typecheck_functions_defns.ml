open Core
open Functions_env
open Parsing
open Type_defns_env
open Type_infer
open Type_infer_types

exception IncorrectFunctionReturnType of string

let current_mutually_recursive_group_id = ref 0

let typecheck_function_signature (types_env : types_env)
    (TFun (_, _, _, _, function_params, _, function_return_type) :
      Parser_ast.function_defn) : unit Or_error.t =
  List.iter function_params ~f:(fun (Ast.Ast_types.TParam (type_expr, _, _)) ->
      Or_error.ok_exn (assert_type_defined type_expr types_env));
  assert_type_defined function_return_type types_env

let typecheck_function_defn (types_env : types_env)
    (constructors_env : constructors_env)
    (function_defns : Parsing.Parser_ast.function_defn list)
    (functions_env : functions_env)
    (TFun
       ( loc,
         group_id,
         fip,
         function_name,
         function_params,
         function_body,
         function_return_type ) as function_defn :
      Parser_ast.function_defn) :
    (functions_env * Typed_ast.function_defn) Or_error.t =
  let open Result in
  typecheck_function_signature types_env function_defn >>= fun () ->
  let extended_functions_env =
    if group_id > !current_mutually_recursive_group_id then (
      current_mutually_recursive_group_id :=
        !current_mutually_recursive_group_id + 1;
      let mutually_recursive_functions_env =
        get_mutually_recursive_function_defns_by_group_id group_id
          function_defns
      in
      let mutually_recusive_with_fip_functions_env =
        add_fip_function_defns_to_functions_env mutually_recursive_functions_env
      in
      mutually_recusive_with_fip_functions_env @ functions_env)
    else functions_env
  in
  let function_signature_type_expr =
    function_return_type
    :: List.map function_params
         ~f:(fun (Ast.Ast_types.TParam (function_param_type, _, _)) ->
           function_param_type)
  in
  let type_scheme_assoc_list =
    get_type_scheme_assoc_list function_signature_type_expr
  in
  let function_typing_context : typing_context =
    List.map function_params
      ~f:(fun
          (Ast.Ast_types.TParam (function_param_type, function_param_var, _)) ->
        Type_context_env.TypingContextEntry
          ( function_param_var,
            convert_ast_type_to_ty function_param_type type_scheme_assoc_list ))
  in
  type_infer types_env constructors_env extended_functions_env
    function_typing_context function_body ~verbose:false
  >>= fun (typed_function_body, substs) ->
  let typed_function_body_type = Typed_ast.get_expr_type typed_function_body in
  let adjust_function_return_type =
    Or_error.ok_exn
      (convert_ty_to_ast_type
         (ty_subst substs
            (convert_ast_type_to_ty function_return_type type_scheme_assoc_list))
         loc)
  in
  if
    not
      (Ast.Ast_types.equal_type_expr adjust_function_return_type
         typed_function_body_type)
  then
    let error_string =
      Fmt.str "Function return type %s does not match the signature %s - %s"
        (Ast.Ast_types.string_of_type typed_function_body_type)
        (Ast.Ast_types.string_of_type adjust_function_return_type)
        (Ast.Ast_types.string_of_loc loc)
    in
    Or_error.of_exn (IncorrectFunctionReturnType error_string)
  else
    Ok
      ( extended_functions_env,
        Typed_ast.TFun
          ( loc,
            adjust_function_return_type,
            group_id,
            fip,
            function_name,
            function_params,
            typed_function_body ) )

let rec typecheck_functions_defns_wrapper (types_env : types_env)
    (constructors_env : constructors_env) (functions_env : functions_env)
    (typed_ast_function_defns : Typed_ast.function_defn list)
    (functions_defns : Parser_ast.function_defn list) :
    (functions_env * Typed_ast.function_defn list) Or_error.t =
  match functions_defns with
  | [] -> Ok (functions_env, typed_ast_function_defns)
  | function_defn :: functions_defns_tail ->
      let open Result in
      typecheck_function_defn types_env constructors_env functions_defns
        functions_env function_defn
      >>= fun (functions_env, typed_function_defn) ->
      let (TFun (_, _, _, fip_option, _, _, _)) = typed_function_defn in
      let extended_typed_ast_function_defns =
        match fip_option with
        | None -> typed_ast_function_defns @ [ typed_function_defn ]
        | Some (Fip _) ->
            (* TODO: Modify here when you have conversion from Fip_ast to Typed_ast *)
            ignore
              (Or_error.ok_exn
                 (Static_fip.fip typed_function_defn functions_env));
            typed_ast_function_defns @ [ typed_function_defn ]
        | Some (Fbip _) ->
            (* TODO: Modify here when you have conversion from Fip_ast to Typed_ast *)
            ignore
              (Or_error.ok_exn
                 (Static_fbip.fbip typed_function_defn functions_env));
            typed_ast_function_defns @ [ typed_function_defn ]
      in
      typecheck_functions_defns_wrapper types_env constructors_env functions_env
        extended_typed_ast_function_defns functions_defns_tail

let typecheck_functions_defns (types_env : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_defns : Parsing.Parser_ast.function_defn list) :
    (functions_env * Typed_ast.function_defn list) Or_error.t =
  let open Result in
  typecheck_functions_defns_wrapper types_env constructors_env [] []
    functions_defns
  >>= fun (functions_env, typed_function_defns) ->
  Ok (functions_env, typed_function_defns)
