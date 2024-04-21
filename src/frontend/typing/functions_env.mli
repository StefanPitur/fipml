open Ast.Ast_types
open Core

exception FunctionNotFound of string
exception FunctionAlreadyExists of string
exception FunctionMultipleInstancesFound
exception FipFunctionExpected of string

type function_env_entry =
  | FunctionEnvEntry of
      int * fip option * Function_name.t * type_expr list * type_expr
      (** [FunctionEnvEntry] contains [Function_name.t] [param_types] [return_type] [fip allocation credit] *)

type functions_env = function_env_entry list

val assert_function_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val assert_function_not_in_functions_env :
  loc -> Function_name.t -> functions_env -> unit Or_error.t

val get_function_by_name :
  loc -> Function_name.t -> functions_env -> function_env_entry Or_error.t

val get_function_mutually_recursive_group_id :
  loc -> Function_name.t -> functions_env -> int Or_error.t

val get_function_params_type :
  Parsing.Parser_ast.function_defn -> type_expr list

val get_mutually_recursive_function_defns_by_group_id :
  int -> Parsing.Parser_ast.function_defn list -> functions_env

val get_mutually_recursive_functions_env_by_group_id :
  int -> functions_env -> functions_env

val get_function_signature :
  loc -> Function_name.t -> functions_env -> type_expr Or_error.t

val get_fip_function_allocation_credit :
  loc -> Function_name.t -> functions_env -> int Or_error.t

val pprint_functions_env : Format.formatter -> functions_env -> unit

val assert_function_has_required_fip_type :
  loc -> fip -> Function_name.t -> functions_env -> unit Or_error.t
