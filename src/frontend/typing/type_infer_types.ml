open Ast.Ast_types
open Core

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed
exception FailureConvertTyToAstType
exception FunctionExpected

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty list

type subst = string * ty
type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

let rec ty_equal (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | TyVar ty_var1, TyVar ty_var2 -> String.( = ) ty_var1 ty_var2
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TyCustom type1, TyCustom type2 -> Type_name.( = ) type1 type2
  | TyArrow (ty11, ty12), TyArrow (ty21, ty22) ->
      ty_equal ty11 ty21 && ty_equal ty12 ty22
  | TyTuple tys1, TyTuple tys2 -> List.for_all2_exn tys1 tys2 ~f:ty_equal
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
  | TEPoly _ -> exit (-1)
  | TECustom (_, _, custom_type_name) -> TyCustom custom_type_name
  | TEArrow (_, input_type_expr, output_type_expr) ->
      TyArrow
        ( convert_ast_type_to_ty input_type_expr,
          convert_ast_type_to_ty output_type_expr )
  | TETuple (_, type_exprs) ->
      TyTuple (List.map type_exprs ~f:convert_ast_type_to_ty)

let rec convert_ty_to_ast_type (ty : ty) (loc : loc) : type_expr Or_error.t =
  match ty with
  | TyUnit -> Ok (TEUnit loc)
  | TyInt -> Ok (TEInt loc)
  | TyBool -> Ok (TEBool loc)
  | TyCustom custom_type_name -> Ok (TECustom (loc, [], custom_type_name))
  | TyArrow (ty1, ty2) ->
      let open Result in
      convert_ty_to_ast_type ty1 loc >>= fun ast_type1 ->
      convert_ty_to_ast_type ty2 loc >>= fun ast_type2 ->
      Ok (TEArrow (loc, ast_type1, ast_type2))
  | TyTuple tys ->
      let type_exprs =
        List.map tys ~f:(fun ty ->
            Or_error.ok_exn (convert_ty_to_ast_type ty loc))
      in
      Ok (TETuple (loc, type_exprs))
  | TyVar _ -> Ok (TECustom (loc, [], Type_name.of_string "_undefined"))

let pop_last_element_from_list (lst : 'a list) : ('a * 'a list) Or_error.t =
  let reversed_lst = List.rev lst in
  match reversed_lst with
  | [] -> Or_error.of_exn UnableToRemoveLastElementFromEmptyList
  | x :: xs -> Ok (x, List.rev xs)

let get_ty_function_signature (ty : ty) : (ty list * ty) Or_error.t =
  match ty with
  | TyArrow _ ->
      let rec get_ty_function_signature (ty : ty) : ty list =
        match ty with
        | TyArrow (in_ty, out_ty) -> in_ty :: get_ty_function_signature out_ty
        | _ -> [ ty ]
      in
      let ty_return, ty_params =
        Or_error.ok_exn
          (pop_last_element_from_list (get_ty_function_signature ty))
      in
      Ok (ty_params, ty_return)
  | _ -> Or_error.of_exn FunctionExpected
