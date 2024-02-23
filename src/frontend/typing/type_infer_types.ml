open Ast.Ast_types

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
