open Ast.Ast_types
open Core

exception FunctionNotFound of string
exception FunctionAlreadyExists of string
exception FunctionMultipleInstancesFound

type function_env_entry = Function_name.t * type_expr list * type_expr
type functions_env = function_env_entry list

let filter_functions_env_by_name (function_name : Function_name.t)
    (functions_env : functions_env) : functions_env =
  List.filter functions_env ~f:(fun (function_env_entry_name, _, _) ->
      Function_name.( = ) function_name function_env_entry_name)

let assert_function_in_functions_env (loc : loc)
    (function_name : Function_name.t) (functions_env : functions_env) :
    unit Or_error.t =
  let open Result in
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] ->
      let error_string =
        Fmt.str "%s. Function %s not defined" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionNotFound error_string)
  | _ -> Ok ()

let assert_function_not_in_functions_env (loc : loc)
    (function_name : Function_name.t) (functions_env : functions_env) :
    unit Or_error.t =
  let open Result in
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] -> Ok ()
  | _ ->
      let error_string =
        Fmt.str "%s. Duplicate function %s" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionAlreadyExists error_string)

let get_function_by_name (loc : loc) (function_name : Function_name.t)
    (functions_env : functions_env) : function_env_entry Or_error.t =
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] ->
      let error_string =
        Fmt.str "%s. Function %s not defined" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionNotFound error_string)
  | [ matched_function ] -> Ok matched_function
  | _ -> Or_error.of_exn FunctionMultipleInstancesFound
