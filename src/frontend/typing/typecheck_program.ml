open Core
open Parsing

let typecheck_program (Parser_ast.TProg (loc, type_defns, _, _)) :
    Typed_ast.program Or_error.t =
  let open Result in
  Typecheck_type_defns.typecheck_type_defns type_defns
  >>= fun (_, _, typed_ast_type_defns) ->
  (* Typecheck_functions_defns.typecheck_functions_defns types_env constructors_env function_defns
     >>= fun _ -> *)
  Ok
    (Typed_ast.TProg (typed_ast_type_defns, Ast.Ast_types.TEUnit loc, [], None))
