open Core
open Type_infer_types

val generate_constraints :
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  (typing_context * ty * constr list * Pretyped_ast.expr) Or_error.t
(** Call constraints generation on expression *)

val generate_constraints_value_expr :
  Type_defns_env.constructors_env ->
  typing_context ->
  Parsing.Parser_ast.value ->
  verbose:bool ->
  (ty * constr list * Pretyped_ast.value) Or_error.t
