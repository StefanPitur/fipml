open Ast.Ast_types
open Core

exception FunctionNotFound of string
exception FunctionAlreadyExists of string
exception FunctionMultipleInstancesFound

type function_env_entry =
  | FunctionEnvEntry of Function_name.t * type_expr list * type_expr

type functions_env = function_env_entry list

val assert_function_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val assert_function_not_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val get_function_by_name :
  loc -> Function_name.t -> functions_env -> function_env_entry Or_error.t
