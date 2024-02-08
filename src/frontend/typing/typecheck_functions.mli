open Core

(** Type-checks function definitions, returns if successful *)
val typecheck_functions
  :  Type_defns_env.types_env
  -> Type_defns_env.constructors_env
  -> Parsing.Parser_ast.function_defn list
  -> unit Or_error.t
