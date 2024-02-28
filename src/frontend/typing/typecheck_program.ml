open Core
open Parsing
open Type_infer

let typecheck_program
    (Parser_ast.TProg (loc, type_defns, function_defns, main_expr_option)) :
    Typed_ast.program Or_error.t =
  let open Result in
  Typecheck_type_defns.typecheck_type_defns type_defns
  >>= fun (types_env, constructors_env, typed_ast_type_defns) ->
  Typecheck_functions_defns.typecheck_functions_defns types_env constructors_env
    function_defns
  >>= fun (functions_env, typed_function_defns) ->
  (match main_expr_option with
  | None -> Ok (None, Ast.Ast_types.TEUnit loc)
  | Some main_block_expr ->
      type_infer types_env constructors_env functions_env [] main_block_expr
        ~verbose:false
      >>= fun typed_main_block_expr ->
      let (Typed_ast.Block (_, typed_main_block_expr_type, _)) =
        typed_main_block_expr
      in
      Ok (Some typed_main_block_expr, typed_main_block_expr_type))
  >>= fun (typed_main_block_expr_option, typed_main_block_expr_type) ->
  Ok
    (Typed_ast.TProg
       ( typed_ast_type_defns,
         typed_main_block_expr_type,
         typed_function_defns,
         typed_main_block_expr_option ))
