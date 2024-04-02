open Core
open Typing

val generate_fbip :
  Typed_ast.expr -> Functions_env.functions_env -> Fip_ast.expr Or_error.t
(** Given the body of a [fbip] annotate function, creates a [fip_ast] to check the fbip rules *)
