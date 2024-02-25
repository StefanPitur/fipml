open Construct_typed_ast
open Core
open Type_infer_constraints_generator
open Type_infer_constraints_unification
open Parsing

let type_infer (_ : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (Block (loc, exprs) as block_expr : Parser_ast.block_expr) ~(verbose : bool)
    : Typed_ast.block_expr Or_error.t =
  match exprs with
  | [] -> Ok (Typed_ast.Block (loc, Ast.Ast_types.TEUnit loc, []))
  | _ ->
      let open Result in
      generate_constraints_block_expr constructors_env functions_env []
        block_expr ~verbose
      >>= fun (_, _, constraints, pretyped_block_expr) ->
      unify constraints >>= fun substs ->
      construct_typed_ast_block pretyped_block_expr substs
      >>= fun typed_block_expr -> Ok typed_block_expr
