open Core

val typecheck_program :
  Parsing.Parser_ast.program ->
  (Typed_ast.program
  * Functions_env.functions_env
  * Type_defns_env.types_env
  * Type_defns_env.constructors_env
  * Fip_ast.function_defn list)
  Or_error.t
(** Given a parsed AST, perform typechecking on it and return a Typed AST if successful. *)
