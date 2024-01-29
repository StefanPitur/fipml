open Parsing
open Ast
open Type_envs
open Core

(* let rec typecheck_type_constructor_arg type_constructor_arg types_env =
  match type_constructor_arg with
  | TEUnit -> ()
  | TEInt -> ()
  | TEBool -> ()
  | TEOption(type_expr) -> typecheck_type_constructor_arg type_expr types_env
  | TEArrow(in_type_expr, out_type_expr) -> 
    typecheck_type_constructor_arg in_type_expr types_env;
    typecheck_type_constructor_arg out_type_expr types_env
  | TECustom(_) -> () *)


(* let typecheck_type_constructors _ _ = () *)

(* let typecheck_type_defn type_defn types_env = 
  let TType(_, type_name, type_constructors) = type_defn;
  (* if List.mem type_name types_env then raise (TypeRedefinition("type redefinition not allowed")) *)
  typecheck_type_constructors type_constructors (type_name :: types_env);
  () *)

let typecheck_type_defn (type_env : Ast_types.Type_name.t list) (TType(_, type_name, _) : Parsing.Parser_ast.type_defn) : unit Or_error.t =
  assert_custom_type_not_in_types_env type_name type_env


let rec typecheck_type_defns (type_env : Ast_types.Type_name.t list) (type_defns : Parser_ast.type_defn list) : (Typed_ast.type_defn list) Or_error.t =
  match type_defns with
  | [] -> List.iter type_env ~f:(fun t -> print_string ((Ast_types.Type_name.to_string t) ^ "\n")); Ok []
  | (TType(_, type_name, _) as type_defn) :: type_defns ->
    let open Result in
      typecheck_type_defn type_env type_defn
      >>= fun () -> 
        let extended_type_env = type_name :: type_env in
        typecheck_type_defns extended_type_env type_defns
