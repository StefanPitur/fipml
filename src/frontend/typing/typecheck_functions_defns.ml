(* open Core
   open Functions_env
   open Parsing
   open Type_defns_env

   let typecheck_function_defn (types_env : types_env)
       (constructors_env : constructors_env)
       (functions_env : functions_env)
       (TFun(_, function_name, params, function_body, function_return_type) : Parser_ast.function_defn) :
       (functions_env * Typed_ast.function_defn) Or_error.t =



   let rec typecheck_functions_defns_wrapper (types_env : types_env)
       (constructors_env : constructors_env)
       (functions_env : functions_env)
       (typed_ast_function_defns : Typed_ast.function_defn list)
       (functions_defns : Parser_ast.function_defn list) :
       (functions_env * Typed_ast.function_defn list) Or_error.t =
     match functions_defns with
     | [] -> Ok (functions_env, typed_ast_function_defns)
     | function_defn :: functions_defns ->


   let typecheck_functions_defns (_ : Type_defns_env.types_env)
       (_ : Type_defns_env.constructors_env)
       (_ : Parsing.Parser_ast.function_defn list) : unit Or_error.t =
     Ok () *)
