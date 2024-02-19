open Ast.Ast_types
open Type_context_env
open Type_infer


let rec string_of_ty (ty : ty) : string =
  match ty with
  | TyVar var -> "TyVar " ^ var
  | TyUnit -> "TyUnit"
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyOption ty -> "TyOption " ^ string_of_ty ty
  | TyCustom type_name -> "TyCustom " ^ Type_name.to_string type_name
  | TyArrow (in_ty, out_ty) ->
      Fmt.str "TyArrow (%s -> %s)" (string_of_ty in_ty) (string_of_ty out_ty)
  | TyTuple (fst_ty, snd_ty) ->
      Fmt.str "TyTuple (%s, %s)" (string_of_ty fst_ty) (string_of_ty snd_ty)

let pprint_ty (ppf : Format.formatter) (ty : ty) : unit =
  Fmt.pf ppf "%s@." (string_of_ty ty)

let rec pprint_typing_context (ppf : Format.formatter) (typing_context : typing_context) : unit =
  match typing_context with
  | [] -> ()
  | TypingContextEntry(var_name, var_ty) :: typing_context ->
      Fmt.pf ppf "%s : %s@." (Var_name.to_string var_name) (string_of_ty var_ty);
      pprint_typing_context ppf typing_context

let rec pprint_constraints (ppf : Format.formatter) (constraints : constr list) : unit =
  match constraints with
  | [] -> ()
  | (ty_fst, ty_snd) :: constraints ->
      Fmt.pf ppf ("(%s, %s)@.") (string_of_ty ty_fst) (string_of_ty ty_snd);
      pprint_constraints ppf constraints