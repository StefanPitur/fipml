open Ast
open Core

(** Type-checks custom type definitions, returns if successful *)
val typecheck_type_defns : 
  Ast_types.Type_name.t list
  -> Type_envs.constructor_env_entry list
  -> Parsing.Parser_ast.type_defn list
  -> (Ast_types.Type_name.t list * Type_envs.constructor_env_entry list * Typed_ast.type_defn list) Or_error.t
