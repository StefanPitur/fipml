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

module Type_name : ID = StringID
module Var_name : ID = StringID
module Constructor_name : ID = StringID
module Function_name : ID = StringID

(* Type for borrowed elements *)
type borrowed = Borrowed

(* Types of expressions in FipML *)
type type_expr = 
  | TEUnit
  | TEInt
  | TEBool
  | TEOption of type_expr
  | TECustom of string

type param = 
  | TParam of type_expr * Var_name.t * borrowed option

(* Unary operators *)
type unary_op =
  | UnOpNot
  | UnOpNeg
  | UnOpFst
  | UnOpSnd

(* Binary operators *)
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


(* Implementation of helper functions for printing AST *)
let string_of_loc loc = 
  let loc_line = string_of_int loc.Lexing.pos_lnum in
  let loc_position = 
    string_of_int (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1) in
  let loc_string = "Line: " ^ loc_line ^ " / Position: " ^ loc_position in
  loc_string


let rec string_of_type = function
  | TEUnit -> "Unit"
  | TEInt -> "Int"
  | TEBool -> "Bool"
  | TEOption type_name -> (string_of_type type_name) ^ " option"
  | TECustom custom_type_name -> custom_type_name
  

let get_params_type (params : param list) = 
  let get_param_type = function
    | TParam(param_type_expr, _, _) -> param_type_expr
  in
  List.map get_param_type params

let string_of_unary_op = function
  | UnOpNeg -> "-"
  | UnOpNot -> "!"
  | UnOpFst -> "Fst"
  | UnOpSnd -> "Snd"

let string_of_binary_op = function
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpMult -> "*"
  | BinOpDiv -> "/"
  | BinOpMod -> "%"
  | BinOpLt -> "<"
  | BinOpGt -> ">"
  | BinOpLeq -> "<="
  | BinOpGeq -> ">="
  | BinOpEq -> "=="
  | BinOpNeq -> "!="
  | BinOpAnd -> "&&"
  | BinOpOr -> "||"

let string_of_borrowed_option = function
  | None -> ""
  | Some Borrowed -> "Borrowed"
