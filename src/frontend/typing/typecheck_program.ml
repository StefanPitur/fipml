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
  | Some main_expr ->
      type_infer types_env constructors_env functions_env [] main_expr
        ~verbose:false
      >>= fun (typed_main_expr, _) ->
      Ok (Some typed_main_expr, Typed_ast.get_expr_type typed_main_expr))
  >>= fun (typed_main_expr_option, typed_main_expr_type) ->
  Ok
    (Typed_ast.TProg
       ( typed_ast_type_defns,
         typed_main_expr_type,
         typed_function_defns,
         typed_main_expr_option ))
