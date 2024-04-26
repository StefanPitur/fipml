open Core
open Functions_env
open Parsing
open Type_defns_env
open Type_infer
open Type_infer_types

exception IncorrectFunctionReturnType of string
exception MixedPolymorphicTypes

let current_mutually_recursive_group_id = ref 0

let assert_function_polys_are_distinct (typ_scheme_assoc_list : subst list)
    (unique_scheme_assoc_list : subst_unique list)
    (type_expr_scheme_assoc_list : subst_attr list) : unit Or_error.t =
  let typ_set =
    String.Set.of_list
      (List.map typ_scheme_assoc_list ~f:(fun (typ_str, _) -> typ_str))
  in
  let unique_set =
    String.Set.of_list
      (List.map unique_scheme_assoc_list ~f:(fun (unique_str, _) -> unique_str))
  in
  let type_expr_set =
    String.Set.of_list
      (List.map type_expr_scheme_assoc_list ~f:(fun (type_expr_str, _) ->
           type_expr_str))
  in
  match
    Set.are_disjoint typ_set unique_set
    && Set.are_disjoint typ_set type_expr_set
    && Set.are_disjoint unique_set type_expr_set
  with
  | true -> Ok ()
  | false -> Or_error.of_exn MixedPolymorphicTypes

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
  let ( typ_scheme_assoc_list,
        unique_scheme_assoc_list,
        type_expr_scheme_assoc_list ) =
    get_type_expr_scheme_assoc_lists function_signature_type_expr
  in
  assert_function_polys_are_distinct typ_scheme_assoc_list
    unique_scheme_assoc_list type_expr_scheme_assoc_list
  >>= fun _ ->
  let get_ty_unique_from_sharing_analysis =
    get_ty_unique_from_sharing_analysis (get_sharing_analysis function_body)
  in
  let function_typing_context, initial_unique_constraints =
    List.fold function_params ~init:([], [])
      ~f:(fun
          (acc_function_typing_context, acc_unique_constraints)
          (Ast.Ast_types.TParam (function_param_type, function_param_var, _))
        ->
        let ty, ty_unique =
          convert_ast_type_to_ty_attr function_param_type typ_scheme_assoc_list
            unique_scheme_assoc_list type_expr_scheme_assoc_list
        in
        ( Type_context_env.TypingContextEntry
            (function_param_var, (ty, ty_unique))
          :: acc_function_typing_context,
          (get_ty_unique_from_sharing_analysis function_param_var, ty_unique)
          :: acc_unique_constraints ))
  in
  let function_return_ty_attr =
    convert_ast_type_to_ty_attr function_return_type typ_scheme_assoc_list
      unique_scheme_assoc_list type_expr_scheme_assoc_list
  in
  type_infer types_env constructors_env extended_functions_env
    function_typing_context initial_unique_constraints
    (Some function_return_ty_attr) function_body ~verbose:false
  >>= fun (typed_function_body, substs, substs_unique) ->
  let adjust_function_return_type =
    Or_error.ok_exn
      (convert_ty_attr_to_ast_type
         (ty_attr_subst substs substs_unique function_return_ty_attr)
         loc)
  in
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
    (fip_ast_function_defns : Fip_ast.function_defn list)
    (functions_defns : Parser_ast.function_defn list) :
    (functions_env * Typed_ast.function_defn list * Fip_ast.function_defn list)
    Or_error.t =
  match functions_defns with
  | [] -> Ok (functions_env, typed_ast_function_defns, fip_ast_function_defns)
  | function_defn :: functions_defns_tail ->
      let open Result in
      typecheck_function_defn types_env constructors_env functions_defns
        functions_env function_defn
      >>= fun (functions_env, typed_function_defn) ->
      let (TFun (_, _, _, fip_option, _, _, _)) = typed_function_defn in
      let extended_typed_ast_function_defns =
        typed_ast_function_defns @ [ typed_function_defn ]
      in
      let extended_fip_ast_function_defns =
        match fip_option with
        | None -> fip_ast_function_defns
        | Some (Fip _) ->
            let fip_ast_function_defn =
              Or_error.ok_exn (Static_fip.fip typed_function_defn functions_env)
            in
            fip_ast_function_defns @ [ fip_ast_function_defn ]
        | Some (Fbip _) ->
            let fbip_ast_function_defn =
              Or_error.ok_exn
                (Static_fbip.fbip typed_function_defn functions_env)
            in
            fip_ast_function_defns @ [ fbip_ast_function_defn ]
      in
      typecheck_functions_defns_wrapper types_env constructors_env functions_env
        extended_typed_ast_function_defns extended_fip_ast_function_defns
        functions_defns_tail

let typecheck_functions_defns (types_env : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_defns : Parsing.Parser_ast.function_defn list) :
    (functions_env * Typed_ast.function_defn list * Fip_ast.function_defn list)
    Or_error.t =
  let open Result in
  typecheck_functions_defns_wrapper types_env constructors_env [] [] []
    functions_defns
  >>= fun (functions_env, typed_function_defns, fiped_function_defns) ->
  Ok (functions_env, typed_function_defns, fiped_function_defns)
