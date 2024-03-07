(* Keeps track of the position of the token in the input stream *)
type loc = Lexing.position

(* Abstract type for identifiers *)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

(* Differentiate between different identifiers *)
module Type_name : ID
module Var_name : ID
module Constructor_name : ID
module Function_name : ID

(** Type for borrowed elements *)
type borrowed = Borrowed

(** Types of expressions in FipML *)
type type_expr =
  | TEUnit of loc
  | TEInt of loc
  | TEBool of loc
  | TEPoly of loc * string
  | TEOption of loc * type_expr
  | TECustom of loc * Type_name.t
  | TEArrow of loc * type_expr * type_expr

type param = TParam of type_expr * Var_name.t * borrowed option

val get_params_type : param list -> type_expr list

(** Unary operators *)
type unary_op = UnOpNot | UnOpNeg | UnOpFst | UnOpSnd

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

(* Helper function for printing AST *)
val string_of_loc : loc -> string
val string_of_type : type_expr -> string
val string_of_unary_op : unary_op -> string
val string_of_binary_op : binary_op -> string
val string_of_borrowed_option : borrowed option -> string
