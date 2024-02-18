open Core

(** Type-checks custom type definitions, returns if successful *)
val typecheck_type_defns : 
  Parsing.Parser_ast.type_defn list
  -> (Type_envs.types_env * Type_envs.constructors_env * Typed_ast.type_defn list) Or_error.t

(** Pretty-print types_env *)
val pprint_types_env : Format.formatter -> Type_envs.types_env -> unit

(** Pretty-print constructors_env *)
val pprint_constructors_env : Format.formatter -> Type_envs.constructors_env -> unit