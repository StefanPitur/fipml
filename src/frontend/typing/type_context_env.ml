open Ast.Ast_types
open Core

exception VariableNotFound of Var_name.t
exception ShadowingNotSupported

type typing_context_entry = TypingContextEntry of Var_name.t * type_expr
type typing_context = typing_context_entry list

let filter_typing_context_by_var (typing_context : typing_context) (var : Var_name.t)
  : typing_context
  =
  List.filter typing_context ~f:(fun (TypingContextEntry (var_entry, _)) ->
    Var_name.( = ) var var_entry)
;;

let check_typing_context_shadow (typing_context : typing_context) (var : Var_name.t)
  : unit Or_error.t
  =
  match filter_typing_context_by_var typing_context var with
  | [] -> Ok ()
  | _ -> Or_error.of_exn ShadowingNotSupported
;;

let extend_typing_context
  (typing_context : typing_context)
  (var : Var_name.t)
  (var_type : type_expr)
  : typing_context Or_error.t
  =
  let open Result in
  check_typing_context_shadow typing_context var
  >>= fun () -> Ok (TypingContextEntry (var, var_type) :: typing_context)
;;

let get_var_type (typing_context : typing_context) (var : Var_name.t)
  : type_expr Or_error.t
  =
  match filter_typing_context_by_var typing_context var with
  | [] -> Or_error.of_exn (VariableNotFound var)
  | [ TypingContextEntry (_, var_type) ] -> Ok var_type
  | _ -> Or_error.of_exn ShadowingNotSupported
;;

let pprint_typing_context ppf (typing_context : typing_context) : unit =
  List.iter typing_context ~f:(fun (TypingContextEntry (var, var_type)) ->
    Fmt.pf
      ppf
      "var name: %s, var type: %s@."
      (Var_name.to_string var)
      (string_of_type var_type))
;;
