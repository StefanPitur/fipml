open Ast.Ast_types
open Core
open Type_infer_types

let rec string_of_ty (ty : ty) : string =
  match ty with
  | TyVar var -> "TyVar " ^ var
  | TyUnit -> "TyUnit"
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyCustom type_name -> "TyCustom " ^ Type_name.to_string type_name
  | TyArrow (in_ty, out_ty) ->
      Fmt.str "TyArrow (%s -> %s)" (string_of_ty in_ty) (string_of_ty out_ty)
  | TyTuple tys ->
      Fmt.str "TyTuple (%s)"
        (String.concat ~sep:", " (List.map tys ~f:string_of_ty))

let pprint_ty (ppf : Format.formatter) (ty : ty) : unit =
  Fmt.pf ppf "%s@." (string_of_ty ty)

let rec pprint_typing_context (ppf : Format.formatter)
    (typing_context : typing_context) : unit =
  match typing_context with
  | [] -> ()
  | TypingContextEntry (var_name, var_ty) :: typing_context ->
      Fmt.pf ppf "%s : %s@." (Var_name.to_string var_name) (string_of_ty var_ty);
      pprint_typing_context ppf typing_context

let rec pprint_constraints (ppf : Format.formatter) (constraints : constr list)
    : unit =
  match constraints with
  | [] -> ()
  | (ty_fst, ty_snd) :: constraints ->
      Fmt.pf ppf "(%s, %s)@." (string_of_ty ty_fst) (string_of_ty ty_snd);
      pprint_constraints ppf constraints

let pprint_type_infer_expr_verbose (ppf : Format.formatter) ~(verbose : bool)
    (expr : Parsing.Parser_ast.expr) (typing_context : typing_context)
    (expr_ty : ty) (expr_constraints : constr list) : unit =
  if not verbose then ()
  else (
    Fmt.pf ppf "Actual expr:@.";
    Parsing.Pprint_parser_ast.pprint_expr ppf ~indent:"" expr;
    Fmt.pf ppf "\n=> Typing Context:@.";
    pprint_typing_context ppf typing_context;
    Fmt.pf ppf "=> Expr Ty:@.";
    pprint_ty ppf expr_ty;
    Fmt.pf ppf "=> Expr Constraints:@.";
    pprint_constraints ppf expr_constraints;
    Fmt.pf ppf "-------------------------\n@.")

let pprint_type_infer_value_verbose (ppf : Format.formatter) ~(verbose : bool)
    (value : Parsing.Parser_ast.value) (value_ty : ty)
    (value_constraints : constr list) : unit =
  if not verbose then ()
  else (
    Fmt.pf ppf "Actual value:@.";
    Parsing.Pprint_parser_ast.pprint_value ppf ~indent:"" value;
    Fmt.pf ppf "=> Value Ty:@.";
    pprint_ty ppf value_ty;
    Fmt.pf ppf "=> Value Constraints:@.";
    pprint_constraints ppf value_constraints;
    Fmt.pf ppf "-------------------------\n@.")

let pprint_substs (ppf : Format.formatter) (substs : subst list) : unit =
  List.iter substs ~f:(fun (ty_var, ty_subst) ->
      Fmt.pf ppf "Type Variable - %s, Type - %s@." ty_var
        (string_of_ty ty_subst))
