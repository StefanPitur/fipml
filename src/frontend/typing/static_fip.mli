open Core

val fip :
  Typed_ast.function_defn ->
  Functions_env.functions_env ->
  Fip_ast.expr Or_error.t
(** Given a [Typed_ast.function_defn] perform fip check on it and return fip-ed expr if correct. *)
