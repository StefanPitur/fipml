open Core
open Parsing
open Type_infer

let typecheck_program
    (Parser_ast.TProg (loc, type_defns, function_defns, main_expr_option)) :
    (Typed_ast.program
    * Functions_env.functions_env
    * Fip_ast.function_defn list)
    Or_error.t =
  let open Result in
  Typecheck_type_defns.typecheck_type_defns type_defns
  >>= fun (types_env, constructors_env, typed_ast_type_defns) ->
  Typecheck_functions_defns.typecheck_functions_defns types_env constructors_env
    function_defns
  >>= fun (functions_env, typed_function_defns, fiped_function_defns) ->
  (match main_expr_option with
  | None ->
      Ok
        ( None,
          Ast.Ast_types.TAttr
            (loc, Ast.Ast_types.TEUnit loc, Ast.Ast_types.Shared loc) )
  | Some main_expr ->
      type_infer types_env constructors_env functions_env [] [] None main_expr
        ~verbose:false
      >>= fun (typed_main_expr, _, _) ->
      Ok (Some typed_main_expr, Typed_ast.get_expr_type typed_main_expr))
  >>= fun (typed_main_expr_option, typed_main_expr_type) ->
  Ok
    ( Typed_ast.TProg
        ( typed_ast_type_defns,
          typed_main_expr_type,
          typed_function_defns,
          typed_main_expr_option ),
      functions_env,
      fiped_function_defns )
