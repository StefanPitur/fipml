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
  | TyPoly of string list * ty
  | TyCustom of ty list * Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty list

type subst = string * ty
type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

let rec occurs (ty_var_name : string) = function
  | TyInt | TyBool | TyUnit -> false
  | TyVar type_var_2 -> String.( = ) ty_var_name type_var_2
  | TyCustom _ -> false
  | TyArrow (ty1, ty2) -> occurs ty_var_name ty1 || occurs ty_var_name ty2
  | TyTuple tys -> List.exists tys ~f:(occurs ty_var_name)
  | TyPoly _ ->
      raise
        (Invalid_argument "did not expect poly to be in the constraints at all")

let rec ty_subst (substs : subst list) (ty : ty) =
  match ty with
  | (TyInt | TyBool | TyUnit) as ty -> ty
  | TyVar type_var -> (
      match List.Assoc.find ~equal:String.( = ) substs type_var with
      | Some subst_ty -> subst_ty
      | None -> TyVar type_var)
  | TyPoly (poly_params, ty) -> TyPoly (poly_params, ty_subst substs ty)
  | TyCustom (ty_custom_args, type_name) ->
      TyCustom (List.map ty_custom_args ~f:(ty_subst substs), type_name)
  | TyArrow (ty1, ty2) -> TyArrow (ty_subst substs ty1, ty_subst substs ty2)
  | TyTuple tys -> TyTuple (List.map tys ~f:(ty_subst substs))

let ty_subst_context (typing_context : typing_context) (substs : subst list) :
    typing_context =
  List.map typing_context ~f:(fun (TypingContextEntry (var_name, var_ty)) ->
      Type_context_env.TypingContextEntry (var_name, ty_subst substs var_ty))

let rec ty_equal (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | TyVar ty_var1, TyVar ty_var2 -> String.( = ) ty_var1 ty_var2
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TyCustom (ty_custom_args1, type1), TyCustom (ty_custom_args2, type2) ->
      Type_name.( = ) type1 type2
      && List.for_all2_exn ty_custom_args1 ty_custom_args2 ~f:ty_equal
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
  | TEPoly _ ->
      print_string "converted TEPoly to TyPoly - check if correct\n";
      fresh ()
  | TECustom (_, custom_type_args, custom_type_name) ->
      let ty_custom_args =
        List.map custom_type_args ~f:convert_ast_type_to_ty
      in
      TyCustom (ty_custom_args, custom_type_name)
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
  | TyCustom (ty_custom_args, custom_type_name) ->
      let custom_type_args =
        List.map ty_custom_args ~f:(fun ty_custom_arg ->
            Or_error.ok_exn (convert_ty_to_ast_type ty_custom_arg loc))
      in
      Ok (TECustom (loc, custom_type_args, custom_type_name))
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
  | TyVar ty_var -> Ok (TEPoly (loc, ty_var))
  | TyPoly _ -> Or_error.of_exn FailureConvertTyToAstType

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
