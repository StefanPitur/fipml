open Ast.Ast_types
open Core

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed
exception FailureConvertTyToAstType

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty list

type subst = string * ty
type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

val convert_ast_type_to_ty : type_expr -> ty
(** Converts AST type into Ty used for type inference *)

val convert_ty_to_ast_type : ty -> loc -> type_expr Or_error.t
(** Converts Ty into AST type *)

val fresh : unit -> ty
(** Generates new fresh ty variable for type inference *)

val pop_last_element_from_list : 'a list -> ('a * 'a list) Or_error.t
(** Pops last element from list *)

val ty_equal : ty -> ty -> bool
(** Checks if the two provides tys are equal *)

val get_ty_function_signature : ty -> (ty list * ty) Or_error.t
(** Given a function encoded in [ty], returns its signature *)
