open Core
open Type_infer_types

val generate_constraints :
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  (typing_context * ty * constr list) Or_error.t
(** Call constraints generation on expression *)

val generate_constraints_block_expr :
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.block_expr ->
  verbose:bool ->
  (typing_context * ty * constr list) Or_error.t
