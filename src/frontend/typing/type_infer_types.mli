open Ast.Ast_types
open Core

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed
exception FailureConvertTyToAstType

type ty_unique = TyVarUnique of string | TyShared | TyUnique

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyPoly of string list * ty
  | TyCustom of ty list * ty_unique list * ty_attr list * Type_name.t
  | TyArrow of ty_attr * ty_attr
  | TyTuple of ty_attr list

and ty_attr = ty * ty_unique

type subst = string * ty
type subst_unique = string * ty_unique
type subst_attr = string * (ty * ty_unique)
type constr = ty * ty
type constr_unique = ty_unique * ty_unique
type typing_context = ty_attr Type_context_env.typing_context

module SharingAnalysisMap : Map.S with type Key.t = Var_name.t

val occurs : string -> ty -> bool
(** [occurs "t1" t] returns whether or not [TyVar "t1"] appears in [t]. *)

val ty_subst : subst list -> subst_unique list -> ty -> ty
(** [ty_subst [(t1, ty1); ...; (tk, tyk)] t] replaces in type [t] type variables [ti] with [tyi] *)

val ty_unique_subst : subst_unique list -> ty_unique -> ty_unique
(** [ty_unique_subst [(u1, ty_unique1); ...; (uk, ty_uniquek)] u] replaces in the uniqueness [u] every uniqueness variable [ui] with [ty_uniquei] *)

val ty_attr_subst : subst list -> subst_unique list -> ty_attr -> ty_attr

val apply_substs_unique_to_substs :
  subst_unique list -> subst list -> subst list
(** Apply [substs_unique] to partial [substs] so they are fully resolved. *)

val ty_subst_context :
  typing_context -> subst list -> subst_unique list -> typing_context
(** Apply the substitutions to all the types within the typing context. *)

val convert_ast_type_to_ty_attr :
  type_expr -> subst list -> subst_unique list -> subst_attr list -> ty_attr
(** Converts AST type into Ty used for type inference *)

val convert_ty_attr_to_ast_type : ty_attr -> loc -> type_expr Or_error.t
(** Converts Ty into AST type *)

val fresh : unit -> ty
(** Generates new fresh ty variable for type inference *)

val fresh_unique : unit -> ty_unique
(** Generates new free ty_unique variable for type inference *)

val pop_last_element_from_list : 'a list -> ('a * 'a list) Or_error.t
(** Pops last element from list *)

val ty_equal : ty -> ty -> bool
(** Checks if the two provides tys are equal *)

val ty_unique_equal : ty_unique -> ty_unique -> bool
val ty_attr_equal : ty_attr -> ty_attr -> bool

val get_ty_attr_function_signature :
  ty_attr -> (ty_attr list * ty_attr) Or_error.t
(** Given a function encoded in [ty], returns its signature *)

val get_type_expr_scheme_assoc_lists :
  type_expr list -> subst list * subst_unique list * subst_attr list
(** Given [type_expr list], return the association lists for its type variables within the type scheme for typ, uniqueness and type_expr. *)

val get_sharing_analysis : Parsing.Parser_ast.expr -> int SharingAnalysisMap.t

val get_ty_unique_from_sharing_analysis :
  int SharingAnalysisMap.t -> Var_name.t -> ty_unique
