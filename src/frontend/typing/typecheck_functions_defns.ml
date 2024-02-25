open Core
open Functions_env
open Parsing
open Type_defns_env
open Type_infer

let typecheck_function_signature (types_env : types_env)
    (TFun (_, _, function_params, _, function_return_type) :
      Parser_ast.function_defn) : unit Or_error.t =
  List.iter function_params ~f:(fun (Ast.Ast_types.TParam (type_expr, _, _)) ->
      Or_error.ok_exn (assert_type_defined type_expr types_env));
  assert_type_defined function_return_type types_env

let typecheck_function_defn (types_env : types_env)
    (constructors_env : constructors_env) (functions_env : functions_env)
    (TFun
       (loc, function_name, function_params, function_body, function_return_type)
     as function_defn :
      Parser_ast.function_defn) :
    (functions_env * Typed_ast.function_defn) Or_error.t =
  let open Result in
  typecheck_function_signature types_env function_defn >>= fun _ ->
  let function_params_types =
    List.map function_params
      ~f:(fun (Ast.Ast_types.TParam (function_param_type, _, _)) ->
        function_param_type)
  in
  type_infer types_env constructors_env functions_env function_body
    ~verbose:false
  >>= fun typed_function_body ->
  Ok
    ( FunctionEnvEntry
        (function_name, function_params_types, function_return_type)
      :: functions_env,
      Typed_ast.TFun
        ( loc,
          function_return_type,
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