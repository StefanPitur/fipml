open Core

val typecheck_type_defns :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Typed_ast.type_defn list ->
  Parsing.Parser_ast.type_defn list ->
  (Type_defns_env.types_env
  * Type_defns_env.constructors_env
  * Typed_ast.type_defn list)
  Or_error.t
(** Type-checks custom type definitions, returns if successful *)
