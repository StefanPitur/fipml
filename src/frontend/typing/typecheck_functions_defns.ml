open Core
open Functions_env
open Parsing
open Type_defns_env
open Type_infer

exception IncorrectFunctionReturnType of string

let typecheck_function_signature (types_env : types_env)
    (TFun (_, _, _, function_params, _, function_return_type) :
      Parser_ast.function_defn) : unit Or_error.t =
  List.iter function_params ~f:(fun (Ast.Ast_types.TParam (type_expr, _, _)) ->
      Or_error.ok_exn (assert_type_defined type_expr types_env));
  assert_type_defined function_return_type types_env

(* Handles all functions as being recursive, regardless of them being annotate as such or not *)
let typecheck_function_defn (types_env : types_env)
    (constructors_env : constructors_env) (functions_env : functions_env)
    (TFun
       ( loc,
         fip,
         function_name,
         function_params,
         function_body,
         function_return_type ) as function_defn :
      Parser_ast.function_defn) :
    (functions_env * Typed_ast.function_defn) Or_error.t =
  let open Result in
  typecheck_function_signature types_env function_defn >>= fun _ ->
  let function_params_types =
    List.map function_params
      ~f:(fun (Ast.Ast_types.TParam (function_param_type, _, _)) ->
        function_param_type)
  in
  let extended_function_env =
    FunctionEnvEntry
      (fip, function_name, function_params_types, function_return_type)
    :: functions_env
  in
  let function_typing_context : Type_infer_types.typing_context =
    List.map function_params
      ~f:(fun
          (Ast.Ast_types.TParam (function_param_type, function_param_var, _)) ->
        Type_context_env.TypingContextEntry
          ( function_param_var,
            Type_infer_types.convert_ast_type_to_ty function_param_type ))
  in
  type_infer types_env constructors_env extended_function_env
    function_typing_context function_body ~verbose:false
  >>= fun typed_function_body ->
  let typed_function_body_type = Typed_ast.get_expr_type typed_function_body in
  if
    not
      (Ast.Ast_types.equal_type_expr function_return_type
         typed_function_body_type)
  then
    let error_string =
      Fmt.str "Function return type %s does not match the signature %s"
        (Ast.Ast_types.string_of_type typed_function_body_type)
        (Ast.Ast_types.string_of_type function_return_type)
    in
    Or_error.of_exn (IncorrectFunctionReturnType error_string)
  else
    Ok
      ( extended_function_env,
        Typed_ast.TFun
          ( loc,
            function_return_type,
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
  | function_defn :: functions_defns ->
      let open Result in
      typecheck_function_defn types_env constructors_env functions_env
        function_defn
      >>= fun (functions_env, typed_function_defn) ->
      typecheck_functions_defns_wrapper types_env constructors_env functions_env
        (typed_function_defn :: typed_ast_function_defns)
        functions_defns

let typecheck_functions_defns (types_env : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_defns : Parsing.Parser_ast.function_defn list) :
    (functions_env * Typed_ast.function_defn list) Or_error.t =
  typecheck_functions_defns_wrapper types_env constructors_env [] []
    functions_defns
