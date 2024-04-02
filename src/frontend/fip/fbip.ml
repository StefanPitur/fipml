open Core
open Typing

exception UnableFbipCheck

let generate_fbip (_ : Typed_ast.expr) (_ : Functions_env.functions_env) :
    Fip_ast.expr Or_error.t =
  Or_error.of_exn UnableFbipCheck
