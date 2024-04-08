open Ast.Ast_types
open Core

exception FunctionNotFound of string
exception FunctionAlreadyExists of string
exception FunctionMultipleInstancesFound
exception FipFunctionExpected of string

type function_env_entry =
  | FunctionEnvEntry of
      fip option * Function_name.t * type_expr list * type_expr
      (** [FunctionEnvEntry] contains [Function_name.t] [param_types] [return_type] [fip allocation credit] *)

type functions_env = function_env_entry list

val assert_function_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val assert_function_not_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val get_function_by_name :
  loc -> Function_name.t -> functions_env -> function_env_entry Or_error.t

val get_function_signature :
  loc -> Function_name.t -> functions_env -> type_expr Or_error.t

val get_fip_function_allocation_credit :
  loc -> Function_name.t -> functions_env -> int Or_error.t
