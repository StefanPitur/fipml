open Core

exception PolyExpected of string

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
type poly = Poly of loc * string
type uniqueness = Unique of loc | Shared of loc | PolyUnique of loc * poly

type typ =
  | TEUnit of loc
  | TEInt of loc
  | TEBool of loc
  | TEPoly of loc * poly
  | TECustom of loc * typ list * uniqueness list * type_expr list * Type_name.t
  | TEArrow of loc * type_expr * type_expr
  | TETuple of loc * type_expr list

and type_expr = TAttr of loc * typ * uniqueness | TPoly of poly

let is_primitive (type_expr : type_expr) : bool =
  match type_expr with
  | TPoly _ -> false
  | TAttr (_, typ, _) -> (
      match typ with TEUnit _ | TEInt _ | TEBool _ -> true | _ -> false)

let equal_borrowed_option (borrowed_option1 : borrowed option)
    (borrowed_option2 : borrowed option) : bool =
  match (borrowed_option1, borrowed_option2) with
  | None, None -> true
  | Some _, Some _ -> true
  | _ -> false

let equal_poly (poly1 : poly) (poly2 : poly) : bool =
  match (poly1, poly2) with
  | Poly (_, poly1), Poly (_, poly2) -> String.( = ) poly1 poly2

let equal_uniqueness (uniqueness1 : uniqueness) (uniqueness2 : uniqueness) :
    bool =
  match (uniqueness1, uniqueness2) with
  | Shared _, Shared _ -> true
  | Unique _, Unique _ -> true
  | PolyUnique (_, poly_unique1), PolyUnique (_, poly_unique2) ->
      equal_poly poly_unique1 poly_unique2
  | _ -> false

let rec equal_type_expr (type_expr1 : type_expr) (type_expr2 : type_expr) : bool
    =
  match (type_expr1, type_expr2) with
  | TPoly poly1, TPoly poly2 -> equal_poly poly1 poly2
  | TAttr (_, typ1, uniqueness1), TAttr (_, typ2, uniqueness2) ->
      equal_uniqueness uniqueness1 uniqueness2 && equal_typ typ1 typ2
  | _ -> false

and equal_typ (typ1 : typ) (typ2 : typ) : bool =
  match (typ1, typ2) with
  | TEUnit _, TEUnit _ | TEInt _, TEInt _ | TEBool _, TEBool _ -> true
  | ( TECustom
        ( _,
          typ_type_polys1,
          uniqueness_type_polys1,
          type_expr_type_polys1,
          custom_type1 ),
      TECustom
        ( _,
          typ_type_polys2,
          uniqueness_type_polys2,
          type_expr_type_polys2,
          custom_type2 ) )
    when Int.( = ) (List.length typ_type_polys1) (List.length typ_type_polys2)
         && Int.( = )
              (List.length uniqueness_type_polys1)
              (List.length uniqueness_type_polys2)
         && Int.( = )
              (List.length type_expr_type_polys1)
              (List.length type_expr_type_polys2)
         && Type_name.( = ) custom_type1 custom_type2 ->
      List.for_all2_exn typ_type_polys1 typ_type_polys2 ~f:equal_typ
      && List.for_all2_exn uniqueness_type_polys1 uniqueness_type_polys2
           ~f:equal_uniqueness
      && List.for_all2_exn type_expr_type_polys1 type_expr_type_polys2
           ~f:equal_type_expr
  | ( TEArrow (_, in_type_expr1, out_type_expr1),
      TEArrow (_, in_type_expr2, out_type_expr2) ) ->
      equal_type_expr in_type_expr1 in_type_expr2
      && equal_type_expr out_type_expr1 out_type_expr2
  | TETuple (_, type_exprs1), TETuple (_, type_exprs2) ->
      List.for_all2_exn type_exprs1 type_exprs2 ~f:equal_type_expr
  | TEPoly (_, poly_id1), TEPoly (_, poly_id2) -> equal_poly poly_id1 poly_id2
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

let get_poly_loc (poly : poly) : loc =
  let (Poly (loc, _)) = poly in
  loc

let get_typ_loc (typ : typ) : loc =
  match typ with
  | TEUnit loc -> loc
  | TEInt loc -> loc
  | TEBool loc -> loc
  | TEPoly (loc, _) -> loc
  | TECustom (loc, _, _, _, _) -> loc
  | TEArrow (loc, _, _) -> loc
  | TETuple (loc, _) -> loc

let get_uniqueness_loc (uniqueness : uniqueness) : loc =
  match uniqueness with
  | Unique loc -> loc
  | Shared loc -> loc
  | PolyUnique (loc, _) -> loc

let get_loc (type_expr : type_expr) : loc =
  match type_expr with TPoly (Poly (loc, _)) | TAttr (loc, _, _) -> loc

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

let string_of_poly = function Poly (_, poly) -> poly

let string_of_uniqueness = function
  | Unique _ -> "unique"
  | Shared _ -> "shared"
  | PolyUnique (_, poly_unique) -> string_of_poly poly_unique

let rec string_of_typ = function
  | TEUnit _ -> "Unit"
  | TEInt _ -> "Int"
  | TEBool _ -> "Bool"
  | TEPoly (_, poly_id) -> string_of_poly poly_id
  | TECustom
      ( _,
        typ_type_polys,
        uniqueness_type_polys,
        type_expr_type_polys,
        custom_type_name ) ->
      let typ_polys_id = List.map typ_type_polys ~f:string_of_typ in
      let uniqueness_polys_id =
        List.map uniqueness_type_polys ~f:string_of_uniqueness
      in
      let type_expr_polys_id =
        List.map type_expr_type_polys ~f:string_of_type
      in
      Fmt.str "(%s ; %s ; %s) %s"
        (String.concat ~sep:", " typ_polys_id)
        (String.concat ~sep:", " uniqueness_polys_id)
        (String.concat ~sep:", " type_expr_polys_id)
        (Type_name.to_string custom_type_name)
  | TEArrow (_, in_type, out_type) ->
      let in_type_string = string_of_type in_type in
      let out_type_string = string_of_type out_type in
      Fmt.str "(%s -> %s)" in_type_string out_type_string
  | TETuple (_, type_exprs) ->
      let type_exprs_strings = List.map type_exprs ~f:string_of_type in
      Fmt.str "(%s)" (String.concat ~sep:" * " type_exprs_strings)

and string_of_type (type_expr : type_expr) =
  match type_expr with
  | TPoly poly -> string_of_poly poly
  | TAttr (_, typ, uniqueness) ->
      string_of_typ typ ^ " @ " ^ string_of_uniqueness uniqueness

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

let convert_typ_to_poly (typ : typ) : poly Or_error.t =
  match typ with
  | TEPoly (_, poly) -> Ok poly
  | _ -> Or_error.of_exn (PolyExpected (string_of_loc (get_typ_loc typ)))

let convert_uniqueness_to_poly (uniqueness : uniqueness) : poly Or_error.t =
  match uniqueness with
  | PolyUnique (_, poly) -> Ok poly
  | _ ->
      Or_error.of_exn
        (PolyExpected (string_of_loc (get_uniqueness_loc uniqueness)))

let convert_type_expr_to_poly (type_expr : type_expr) : poly Or_error.t =
  match type_expr with
  | TPoly poly -> Ok poly
  | _ -> Or_error.of_exn (PolyExpected (string_of_loc (get_loc type_expr)))

let assert_type_expr_is_unique (type_expr : type_expr) : unit Or_error.t =
  match type_expr with
  | TPoly (Poly (loc, _))
  | TAttr (loc, _, PolyUnique _)
  | TAttr (loc, _, Shared _) ->
      let error_string =
        Fmt.str
          "%s - Type Expr uniqueness attribute is not strictly unique - %s"
          (string_of_loc loc) (string_of_type type_expr)
      in
      Or_error.of_exn (Invalid_argument error_string)
  | _ -> Ok ()
