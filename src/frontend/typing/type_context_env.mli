open Ast.Ast_types
open Core

exception VariableNotFound of Var_name.t
exception ShadowingNotSupported

type typing_context_entry = TypingContextEntry of Var_name.t * type_expr
type typing_context = typing_context_entry list

(** Extend the typing context with a new entry *)
val extend_typing_context
  :  typing_context
  -> Var_name.t
  -> type_expr
  -> typing_context Or_error.t

(** Get type for variable in typing context *)
val get_var_type : typing_context -> Var_name.t -> type_expr Or_error.t

(** Pretty-print typing context *)
val pprint_typing_context : Format.formatter -> typing_context -> unit
