open Borrowed_context
open Core

val fip_rules_check_expr :
  Typed_ast.expr ->
  BorrowedSet.t ->
  Functions_env.functions_env ->
  Fip_ast.expr Or_error.t
(** Pass a fip function's body, checks if it is actually fip or not. *)

val fip_rules_check_value :
  Typed_ast.value -> BorrowedSet.t -> Fip_ast.value Or_error.t
(** Pass a fip value, checks if it is actually fip or not. *)
