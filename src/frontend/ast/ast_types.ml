open Core

type loc = Lexing.position

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

module StringID : ID = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
  let compare = String.compare
  let sexp_of_t x = Sexp.Atom x

  let t_of_sexp = function
    | Sexp.Atom s -> of_string s
    | _ -> failwith "Invalid sexp for StringID.t"

  let hash x = Hashtbl.hash x
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
  | TETuple of loc * type_expr list

let rec equal_type_expr (type_expr1 : type_expr) (type_expr2 : type_expr) : bool
    =
  match (type_expr1, type_expr2) with
  | TEUnit _, TEUnit _ | TEInt _, TEInt _ | TEBool _, TEBool _ -> true
  | ( TECustom (_, custom_type_polys1, custom_type1),
      TECustom (_, custom_type_polys2, custom_type2) ) ->
      Int.( = )
        (List.length custom_type_polys1)
        (List.length custom_type_polys2)
      && Type_name.( = ) custom_type1 custom_type2
  | ( TEArrow (_, in_type_expr1, out_type_expr1),
      TEArrow (_, in_type_expr2, out_type_expr2) ) ->
      equal_type_expr in_type_expr1 in_type_expr2
      && equal_type_expr out_type_expr1 out_type_expr2
  | TETuple (_, type_exprs1), TETuple (_, type_exprs2) ->
      List.for_all2_exn type_exprs1 type_exprs2 ~f:equal_type_expr
  | _ -> false

type param = TParam of type_expr * Var_name.t * borrowed option
type fip = Fip of int | Fbip of int

(* Unary operators *)
type unary_op = UnOpNot | UnOpNeg

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

let get_loc (type_expr : type_expr) : loc =
  match type_expr with
  | TEUnit loc -> loc
  | TEInt loc -> loc
  | TEBool loc -> loc
  | TEPoly (loc, _) -> loc
  | TECustom (loc, _, _) -> loc
  | TEArrow (loc, _, _) -> loc
  | TETuple (loc, _) -> loc

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
  | TEPoly (_, poly_id) -> Fmt.str "%s" poly_id
  | TECustom (_, custom_poly_params, custom_type_name) ->
      let custom_poly_params_string =
        match custom_poly_params with
        | [] -> ""
        | _ ->
            let custom_poly_params_strings =
              List.map ~f:string_of_type custom_poly_params
            in
            "(" ^ String.concat ~sep:", " custom_poly_params_strings ^ ") "
      in
      custom_poly_params_string ^ Type_name.to_string custom_type_name
  | TEArrow (_, in_type, out_type) ->
      let in_type_string = string_of_type in_type in
      let out_type_string = string_of_type out_type in
      Fmt.str "(%s -> %s)" in_type_string out_type_string
  | TETuple (_, type_exprs) ->
      let type_exprs_strings = List.map type_exprs ~f:string_of_type in
      Fmt.str "(%s)" (String.concat ~sep:" * " type_exprs_strings)

let get_params_type (params : param list) =
  let get_param_type = function
    | TParam (param_type_expr, _, _) -> param_type_expr
  in
  List.map params ~f:get_param_type

let string_of_unary_op = function UnOpNeg -> "-" | UnOpNot -> "!"

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
  | None -> "Owned"
  | Some Borrowed -> "Borrowed"

let string_of_fip_option = function
  | None -> ""
  | Some (Fip n) -> Fmt.str "Fip(%s) " (string_of_int n)
  | Some (Fbip n) -> Fmt.str "Fbip(%s) " (string_of_int n)
