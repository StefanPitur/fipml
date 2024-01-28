open Core
open Parsing

let typecheck_program (Parser_ast.TProg (type_defns, _, _)) :  (Typed_ast.program Or_error.t) = 
  let open Result in
  Typecheck_type_defns.typecheck_type_defns [] type_defns
  >>= fun _ -> Ok (Typed_ast.TProg([], [], None))

let pprint_typed_ast _ _ = ()