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

(* val pprint_typing_context : Format.formatter -> 'a typing_context -> unit *)
(** Pretty-print typing context *)
