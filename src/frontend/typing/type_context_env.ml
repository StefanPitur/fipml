open Ast.Ast_types
open Core

exception ContextsNotDisjoint
exception ShadowingNotSupported
exception VariableNotFound of Var_name.t

type 'a typing_context_entry = TypingContextEntry of Var_name.t * 'a
type 'a typing_context = 'a typing_context_entry list

let filter_typing_context_by_var (typing_context : 'a typing_context)
    (var : Var_name.t) : 'a typing_context =
  List.filter typing_context ~f:(fun (TypingContextEntry (var_entry, _)) ->
      Var_name.( = ) var var_entry)

let check_typing_context_shadow (typing_context : 'a typing_context)
    (var : Var_name.t) : unit Or_error.t =
  match filter_typing_context_by_var typing_context var with
  | [] -> Ok ()
  | _ -> Or_error.of_exn ShadowingNotSupported

let extend_typing_context (typing_context : 'a typing_context)
    (var : Var_name.t) (var_type : 'a) : 'a typing_context Or_error.t =
  let open Result in
  check_typing_context_shadow typing_context var >>= fun () ->
  Ok (TypingContextEntry (var, var_type) :: typing_context)

let get_var_type (typing_context : 'a typing_context) (var : Var_name.t) :
    'a Or_error.t =
  match filter_typing_context_by_var typing_context var with
  | [] -> Or_error.of_exn (VariableNotFound var)
  | [ TypingContextEntry (_, var_type) ] -> Ok var_type
  | _ -> Or_error.of_exn ShadowingNotSupported

let rec union_disjoint_typing_contexts (typing_context_left : 'a typing_context)
    (typing_context_right : 'a typing_context) : 'a typing_context Or_error.t =
  match typing_context_left with
  | [] -> Ok typing_context_right
  | x :: xs ->
      let is_member =
        List.mem typing_context_right x
          ~equal:(fun
              (TypingContextEntry (var_name_left, _))
              (TypingContextEntry (var_name_right, _))
            -> Var_name.( = ) var_name_left var_name_right)
      in
      if is_member then Or_error.of_exn ContextsNotDisjoint
      else
        let open Result in
        union_disjoint_typing_contexts xs typing_context_right
        >>= fun union_typing_contexts -> Ok (x :: union_typing_contexts)
