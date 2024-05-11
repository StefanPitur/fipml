open Core
open Functions_env

exception IncorrectFunctionReturnType of string

val typecheck_functions_defns :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Parsing.Parser_ast.function_defn list ->
  (functions_env * Typed_ast.function_defn list * Fip_ast.function_defn list)
  Or_error.t
(** Type-checks function definitions, returns if successful *)
