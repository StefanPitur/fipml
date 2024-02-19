open Ast.Ast_types
open Core

exception VariableNotFound of Var_name.t
exception ShadowingNotSupported

type 'a typing_context_entry = TypingContextEntry of Var_name.t * 'a
type 'a typing_context = 'a typing_context_entry list

val extend_typing_context :
  'a typing_context -> Var_name.t -> 'a -> 'a typing_context Or_error.t
(** Extend the typing context with a new entry *)

val get_var_type : 'a typing_context -> Var_name.t -> 'a Or_error.t
(** Get type for variable in typing context *)

val union_disjoint_typing_contexts :
  'a typing_context -> 'a typing_context -> 'a typing_context Or_error.t
(** Union of two disjoint typing contexts *)
