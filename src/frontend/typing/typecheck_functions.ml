open Core

let typecheck_functions
  (_  : Type_envs.types_env)
  (_ : Type_envs.constructors_env)
  (_ : Parsing.Parser_ast.function_defn list)
  : unit Or_error.t =
  Ok ()
