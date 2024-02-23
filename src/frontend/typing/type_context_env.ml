open Ast.Ast_types
open Core

exception VariableNotFound of Var_name.t

type 'a typing_context_entry = TypingContextEntry of Var_name.t * 'a
type 'a typing_context = 'a typing_context_entry list

let filter_typing_context_by_var (typing_context : 'a typing_context)
    (var : Var_name.t) : 'a typing_context =
  List.filter typing_context ~f:(fun (TypingContextEntry (var_entry, _)) ->
      Var_name.( = ) var var_entry)

let extend_typing_context (typing_context : 'a typing_context)
    (var : Var_name.t) (var_type : 'a) : 'a typing_context =
  TypingContextEntry (var, var_type) :: typing_context

let get_var_type (typing_context : 'a typing_context) (var : Var_name.t) :
    'a Or_error.t =
  match filter_typing_context_by_var typing_context var with
  | [] -> Or_error.of_exn (VariableNotFound var)
  | TypingContextEntry (_, var_type) :: _ -> Ok var_type

let rec prepend_typing_contexts (typing_context_left : 'a typing_context)
    (typing_context_right : 'a typing_context) : 'a typing_context =
  match typing_context_left with
  | [] -> typing_context_right
  | x :: xs -> x :: prepend_typing_contexts xs typing_context_right
