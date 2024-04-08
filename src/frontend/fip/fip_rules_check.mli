open Borrowed_context
open Typing

val fip_rules_check_expr :
  Typed_ast.expr -> BorrowedSet.t -> Functions_env.functions_env -> Fip_ast.expr
(** Pass a fip/fbip function's body, checks if it is actually fip or not. *)

val fip_rules_check_value : Typed_ast.value -> BorrowedSet.t -> Fip_ast.value
(** Pass a fip/fbip value, checks if it is actually fip or not. *)
