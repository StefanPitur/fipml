open Format

type loc = Lexing.position

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module StringID : ID = struct
  type t = string
  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
end

module Var_name : ID = StringID
module Type_name : ID = StringID
module Function_name : ID = StringID

(* Types of expressions in FipML *)
type type_expr = 
  | TEInt
  | TEVoid
  | TEString
  | TEBool

(* Type for borrowed elements *)
type borrowed = BORROWED

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




(* Implementation of helper functions for printing AST *)
let string_of_loc loc = 
  let loc_line = string_of_int loc.Lexing.pos_lnum in
  let loc_position = 
    string_of_int (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1) in
  let loc_string = "Line: " ^ loc_line ^ " / Position: " ^ loc_position in
  loc_string


(* TODO *)
(* let string_of_type type_expr = () *)

let string_of_unary_op = function
  | UnOpNeg -> "-"
  | UnOpNot -> "~"

let string_of_binary_op = function
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpMult -> "*"
  | BinOpDiv -> "/"
  | BinOpMod -> "%"
  | BinOpLt -> "<"
  | BinOpLeq -> "<="
  | BinOpGt -> ">"
  | BinOpGeq -> ">="
  | BinOpEq -> "=="
  | BinOpNeq -> "!="
  | BinOpAnd -> "&&"
  | BinOpOr -> "||"
  | BinOpArrow -> "->"

let string_of_borrowed_option = function
  | None -> ""
  | Some _ -> "Borrowed "
