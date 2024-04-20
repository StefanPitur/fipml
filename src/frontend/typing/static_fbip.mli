open Core

val fbip :
  Typed_ast.function_defn ->
  Functions_env.functions_env ->
  Fip_ast.expr Or_error.t
(** Given a [Typed_ast.function_defn] perform fbip check on it and return fbip-ed expr if correct. *)
