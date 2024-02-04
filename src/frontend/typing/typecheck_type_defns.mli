open Core

(** Type-checks custom type definitions, returns if successful *)
val typecheck_type_defns : 
  Type_envs.types_env
  -> Type_envs.constructors_env
  -> Typed_ast.type_defn list
  -> Parsing.Parser_ast.type_defn list
  -> (Type_envs.types_env * Type_envs.constructors_env * Typed_ast.type_defn list) Or_error.t
