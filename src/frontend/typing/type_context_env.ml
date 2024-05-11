open Ast.Ast_types
open Core

exception VariableNotFound of Var_name.t
exception VariableShadowingNotAllowed of Var_name.t

type 'a typing_context_entry = TypingContextEntry of Var_name.t * 'a
type 'a typing_context = 'a typing_context_entry list

let filter_typing_context_by_var (typing_context : 'a typing_context)
    (var : Var_name.t) : 'a typing_context =
  List.filter typing_context ~f:(fun (TypingContextEntry (var_entry, _)) ->
      Var_name.( = ) var var_entry)

let extend_typing_context (typing_context : 'a typing_context)
    (var : Var_name.t) (var_type : 'a) : 'a typing_context Or_error.t =
  match filter_typing_context_by_var typing_context var with
  | [] -> Ok (TypingContextEntry (var, var_type) :: typing_context)
  | _ -> Or_error.of_exn (VariableShadowingNotAllowed var)

let get_var_type (typing_context : 'a typing_context) (var : Var_name.t) :
    'a Or_error.t =
  match filter_typing_context_by_var typing_context var with
  | [] -> Or_error.of_exn (VariableNotFound var)
  | [ TypingContextEntry (_, var_type) ] -> Ok var_type
  | _ -> Or_error.of_exn (VariableShadowingNotAllowed var)

let remove_var_from_typing_context (typing_context : 'a typing_context)
    (var : Var_name.t) : 'a typing_context Or_error.t =
  match
    List.find typing_context ~f:(fun (TypingContextEntry (var_entry, _)) ->
        Var_name.( = ) var var_entry)
  with
  | None -> Or_error.of_exn (VariableNotFound var)
  | Some _ ->
      let rec remove_var typing_context =
        match typing_context with
        | [] -> []
        | (TypingContextEntry (var_entry, _) as tce) :: typing_context ->
            if Var_name.( = ) var var_entry then typing_context
            else tce :: remove_var typing_context
      in
      Ok (remove_var typing_context)

let combine_typing_contexts (typing_context_left : 'a typing_context)
    (typing_context_right : 'a typing_context) : 'a typing_context Or_error.t =
  let combined_typing_context = typing_context_left @ typing_context_right in
  match
    List.find_a_dup combined_typing_context
      ~compare:(fun
          (TypingContextEntry (var1, _)) (TypingContextEntry (var2, _)) ->
        Var_name.compare var1 var2)
  with
  | None -> Ok combined_typing_context
  | Some (TypingContextEntry (dup_var, _)) ->
      Or_error.of_exn (VariableShadowingNotAllowed dup_var)
