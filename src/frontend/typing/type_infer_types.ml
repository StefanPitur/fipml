open Ast.Ast_types
open Core

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyOption of ty
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty * ty

type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

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
  | [] -> Or_error.of_exn (UnableToRemoveLastElementFromEmptyList)
  | x :: xs -> Ok (x, List.rev xs)
