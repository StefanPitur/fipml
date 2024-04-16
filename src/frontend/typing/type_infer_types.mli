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
  | TyPoly of string list * ty
  | TyCustom of ty list * Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty list

type subst = string * ty
type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

val occurs : string -> ty -> bool
(** [occurs "t1" t] returns whether or not [TyVar "t1"] appears in [t]. *)

val ty_subst : subst list -> ty -> ty
(** [ty_subst [(t1, ty1); ...; (tk, tyk)] t] replaces in type [t] type variables [ti] with [tyi] *)

val ty_subst_context : typing_context -> subst list -> typing_context
(** Apply the substitutions to all the types within the typing context. *)

val convert_ast_type_to_ty : type_expr -> (string * ty) list -> ty
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

val get_type_scheme_assoc_list : type_expr list -> (string * ty) list Or_error.t
(** Given a custom type, return a map for it's type variables within the type scheme to TyVars. *)
