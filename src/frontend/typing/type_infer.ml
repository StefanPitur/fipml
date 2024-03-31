open Construct_typed_ast
open Core
open Type_infer_constraints_generator
open Type_infer_constraints_unification
open Parsing

let type_infer (_ : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : Type_infer_types.typing_context) (expr : Parser_ast.expr)
    ~(verbose : bool) : Typed_ast.expr Or_error.t =
  let open Result in
  generate_constraints constructors_env functions_env typing_context expr
    ~verbose
  >>= fun (_, _, constraints, pretyped_expr) ->
  unify constraints >>= fun substs ->
  construct_typed_ast_expr pretyped_expr substs
