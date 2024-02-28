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
  | TyOption of ty
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty * ty

type subst = string * ty
type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

let rec ty_equal (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | TyVar ty_var1, TyVar ty_var2 -> String.( = ) ty_var1 ty_var2
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TyOption ty1, TyOption ty2 -> ty_equal ty1 ty2
  | TyCustom type1, TyCustom type2 -> Type_name.( = ) type1 type2
  | TyArrow (ty11, ty12), TyArrow (ty21, ty22) ->
      ty_equal ty11 ty21 && ty_equal ty12 ty22
  | TyTuple (ty11, ty12), TyTuple (ty21, ty22) ->
      ty_equal ty11 ty21 && ty_equal ty12 ty22
  | _ -> false

let fresh =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    TyVar ("t" ^ string_of_int !index)

let rec convert_ast_type_to_ty (type_expr : type_expr) : ty =
  match type_expr with
  | TEUnit _ -> TyUnit
  | TEInt _ -> TyInt
  | TEBool _ -> TyBool
  | TEOption (_, type_expr) -> TyOption (convert_ast_type_to_ty type_expr)
  | TECustom (_, custom_type_name) -> TyCustom custom_type_name
  | TEArrow (_, input_type_expr, output_type_expr) ->
      TyArrow
        ( convert_ast_type_to_ty input_type_expr,
          convert_ast_type_to_ty output_type_expr )

let rec convert_ty_to_ast_type (ty : ty) (loc : loc) : type_expr Or_error.t =
  match ty with
  | TyUnit -> Ok (TEUnit loc)
  | TyInt -> Ok (TEInt loc)
  | TyBool -> Ok (TEBool loc)
  | TyOption ty ->
      let open Result in
      convert_ty_to_ast_type ty loc >>= fun ast_type ->
      Ok (TEOption (loc, ast_type))
  | TyCustom custom_type_name -> Ok (TECustom (loc, custom_type_name))
  | TyArrow (ty1, ty2) ->
      let open Result in
      convert_ty_to_ast_type ty1 loc >>= fun ast_type1 ->
      convert_ty_to_ast_type ty2 loc >>= fun ast_type2 ->
      Ok (TEArrow (loc, ast_type1, ast_type2))
  | TyVar _ -> Ok (TECustom (loc, Type_name.of_string "_undefined"))
  | _ -> Or_error.of_exn FailureConvertTyToAstType

(* This can be removed by using List.fold2, probably the last one as well *)
let rec zip_lists (list1 : 'a list) (list2 : 'b list) :
    ('a * 'b) list Or_error.t =
  match (list1, list2) with
  | [], [] -> Ok []
  | [], _ | _, [] -> Or_error.of_exn ListsOfDifferentLengths
  | x :: xs, y :: ys ->
      let open Result in
      zip_lists xs ys >>= fun combined_list -> Ok ((x, y) :: combined_list)

let pop_last_element_from_list (lst : 'a list) : ('a * 'a list) Or_error.t =
  let reversed_lst = List.rev lst in
  match reversed_lst with
  | [] -> Or_error.of_exn UnableToRemoveLastElementFromEmptyList
  | x :: xs -> Ok (x, List.rev xs)
