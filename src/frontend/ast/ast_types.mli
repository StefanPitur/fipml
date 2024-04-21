open Core

(* Keeps track of the position of the token in the input stream *)
type loc = Lexing.position

(* Abstract type for identifiers *)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val hash : t -> int
end

(* Differentiate between different identifiers *)
module Type_name : ID
module Var_name : ID
module Constructor_name : ID
module Function_name : ID

(** Type for borrowed elements *)
type borrowed = Borrowed

(** Types of expressions in FipML *)
type poly = Poly of loc * string

type uniqueness = Unique of loc | Shared of loc | PolyUnique of loc * poly

type typ =
  | TEUnit of loc
  | TEInt of loc
  | TEBool of loc
  | TEPoly of loc * poly
  | TECustom of loc * custom_poly_arg list * Type_name.t
  | TEArrow of loc * type_expr * type_expr
  | TETuple of loc * type_expr list

and type_expr = TAttr of loc * typ * uniqueness | TPoly of poly

and custom_poly_arg =
  | CustomArgTypeExpr of type_expr
  | CustomArgUnique of uniqueness
  | CustomArgPoly of poly

val equal_type_expr : type_expr -> type_expr -> bool

type param = TParam of type_expr * Var_name.t * borrowed option
type fip = Fip of int | Fbip of int

val get_params_type : param list -> type_expr list

(** Unary operators *)
type unary_op = UnOpNot | UnOpNeg

(** Binary operators *)
type binary_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpMod
  | BinOpLt
  | BinOpGt
  | BinOpLeq
  | BinOpGeq
  | BinOpEq
  | BinOpNeq
  | BinOpAnd
  | BinOpOr

val get_loc : type_expr -> loc
(** Extract loc from type expression *)

(* Helper function for printing AST *)
val string_of_loc : loc -> string
val string_of_poly : poly -> string
val string_of_uniqueness : uniqueness -> string
val string_of_typ : typ -> string
val string_of_type : type_expr -> string
val string_of_unary_op : unary_op -> string
val string_of_binary_op : binary_op -> string
val string_of_borrowed_option : borrowed option -> string
val string_of_fip_option : fip option -> string
