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
  | TEUnit of loc
  | TEInt of loc
  | TEBool of loc
  | TEPoly of loc * string
  | TECustom of loc * type_expr list * Type_name.t
  | TEArrow of loc * type_expr * type_expr

type param = TParam of type_expr * Var_name.t * borrowed option

(* Unary operators *)
type unary_op = UnOpNot | UnOpNeg | UnOpFst | UnOpSnd

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
  let loc_file = loc.Lexing.pos_fname in
  let loc_line = string_of_int loc.Lexing.pos_lnum in
  let loc_column =
    string_of_int (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)
  in
  let loc_string =
    "File: " ^ loc_file ^ " - Line: " ^ loc_line ^ " - Column: " ^ loc_column
  in
  loc_string

let rec string_of_type = function
  | TEUnit _ -> "Unit"
  | TEInt _ -> "Int"
  | TEBool _ -> "Bool"
  | TEPoly (_, poly) -> Fmt.str "%s" poly
  | TECustom (_, custom_poly_params, custom_type_name) ->
      let custom_poly_params_string =
        match custom_poly_params with
        | [] -> ""
        | _ ->
            let custom_poly_params_strings =
              List.map string_of_type custom_poly_params
            in
            "(" ^ String.concat ", " custom_poly_params_strings ^ ") "
      in
      custom_poly_params_string ^ Type_name.to_string custom_type_name
  | TEArrow (_, in_type, out_type) ->
      let in_type_string = string_of_type in_type in
      let out_type_string = string_of_type out_type in
      Fmt.str "(%s -> %s)" in_type_string out_type_string

let get_params_type (params : param list) =
  let get_param_type = function
    | TParam (param_type_expr, _, _) -> param_type_expr
  in
  List.map get_param_type params

let string_of_unary_op = function
  | UnOpNeg -> "-"
  | UnOpNot -> "!"
  | UnOpFst -> "fst"
  | UnOpSnd -> "snd"

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
