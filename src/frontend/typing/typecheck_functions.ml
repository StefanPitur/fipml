open Core

let typecheck_functions (_ : Type_defns_env.types_env)
    (_ : Type_defns_env.constructors_env)
    (_ : Parsing.Parser_ast.function_defn list) : unit Or_error.t =
  Ok ()
