open Ast.Ast_types
open Core

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyOption of ty
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty * ty

type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

val convert_ast_type_to_ty : type_expr -> ty
(** Converts AST type into Ty used for type inference *)

val fresh : unit -> ty
(** Generates new fresh ty variable for type inference *)

val zip_lists : 'a list -> 'b list -> ('a * 'b) list Or_error.t
(** Zips two lists together so they may be iterated over at the same time *)

val pop_last_element_from_list : 'a list -> ('a * 'a list) Or_error.t
(** Pops last element from list *)
