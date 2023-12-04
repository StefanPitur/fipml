(* Keeps track of the position of the token in the input stream *)
type loc = Lexing.position

(* Abstract type for identifiers *)
module type ID = sig
  type t
  val of_string : string -> t
  val to_string: t -> string
  val ( = ) : t -> t -> bool
end

(* Differentiate between different identifiers *)
module Var_name : ID
module Type_name : ID
module Function_name : ID

(* Type for borrowed elements *)
type borrowed = BORROWED

(* Types of expressions in FipML *)
type type_expr = 
    | TEInt
    | TEVoid
    | TEString
    | TEBool

type param = 
    | TParam of type_expr * Var_name.t * borrowed option

val get_params_type : param list -> type_expr list

(* Unary operators *)
type unary_op =
    | UnOpNot
    | UnOpNeg

(* Binary operators *)
type binary_oper =
    | BinOpPlus
    | BinOpMinus
    | BinOpMult
    | BinOpDiv
    | BinOpMod
    | BinOpLt
    | BinOpLeq
    | BinOpGt
    | BinOpGeq
    | BinOpEq
    | BinOpNeq
    | BinOpAnd
    | BinOpOr
    | BinOpArrow

(* Helper function for printing AST *)
val string_of_loc : loc -> string
val string_of_type : type_expr -> string
val string_of_unary_op : unary_op -> string
val string_of_binary_op : binary_oper -> string
val string_of_borrowed_option : borrowed option -> string
