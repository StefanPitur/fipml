open Core
open Type_infer_constraints_generator
open Parsing.Parser_ast

(*
  Implementation notes:
  - nothing on tuples actually works, need to implement tuples  
  - typing context is not passed between successive expressions in block_expr 
*)

let type_infer (_ : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (Block (_, exprs) : block_expr) ~(verbose : bool) : unit Or_error.t =
  match exprs with
  | [] -> Ok ()
  | expr :: _ ->
      let open Result in
      generate_constraints constructors_env functions_env [] expr ~verbose
      >>= fun _ -> Ok ()
