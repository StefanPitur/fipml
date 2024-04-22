open Ast.Ast_types
open Core
open Type_infer_types

let string_of_ty_unique (ty_unique : ty_unique) : string =
  match ty_unique with
  | TyShared -> "TyShared"
  | TyUnique -> "TyUnique"
  | TyVarUnique var -> "TyVarUnique " ^ var

let rec string_of_ty (ty : ty) : string =
  match ty with
  | TyVar var -> "TyVar " ^ var
  | TyUnit -> "TyUnit"
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyPoly (type_vars, ty) ->
      Fmt.str "TyPoly - for all (%s). %s"
        (String.concat ~sep:", " type_vars)
        (string_of_ty ty)
  | TyCustom (tys, ty_uniques, ty_attrs, type_name) ->
      let tys_string = String.concat ~sep:", " (List.map tys ~f:string_of_ty) in
      let ty_uniques_string =
        String.concat ~sep:", " (List.map ty_uniques ~f:string_of_ty_unique)
      in
      let ty_attrs_string =
        String.concat ~sep:", " (List.map ty_attrs ~f:string_of_ty_attr)
      in
      Fmt.str "TyCustom (%s ; %s ; %s) %s" tys_string ty_uniques_string
        ty_attrs_string
        (Type_name.to_string type_name)
  | TyArrow (in_ty_attr, out_ty_attr) ->
      Fmt.str "TyArrow (%s -> %s)"
        (string_of_ty_attr in_ty_attr)
        (string_of_ty_attr out_ty_attr)
  | TyTuple ty_attrs ->
      Fmt.str "TyTuple (%s)"
        (String.concat ~sep:", " (List.map ty_attrs ~f:string_of_ty_attr))

and string_of_ty_attr ((ty, ty_unique) : ty_attr) : string =
  Fmt.str "TyAttr - %s @ %s" (string_of_ty ty) (string_of_ty_unique ty_unique)

let pprint_ty_attr (ppf : Format.formatter) (ty_attr : ty_attr) : unit =
  Fmt.pf ppf "%s@." (string_of_ty_attr ty_attr)

let rec pprint_typing_context (ppf : Format.formatter)
    (typing_context : typing_context) : unit =
  match typing_context with
  | [] -> ()
  | TypingContextEntry (var_name, var_ty_attr) :: typing_context ->
      Fmt.pf ppf "%s : %s@."
        (Var_name.to_string var_name)
        (string_of_ty_attr var_ty_attr);
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
    (expr_ty_attr : ty_attr) (expr_constraints : constr list) : unit =
  if not verbose then ()
  else (
    Fmt.pf ppf "Actual expr:@.";
    Parsing.Pprint_parser_ast.pprint_expr ppf ~indent:"" expr;
    Fmt.pf ppf "\n=> Typing Context:@.";
    pprint_typing_context ppf typing_context;
    Fmt.pf ppf "=> Expr TyAttr:@.";
    pprint_ty_attr ppf expr_ty_attr;
    Fmt.pf ppf "=> Expr Constraints:@.";
    pprint_constraints ppf expr_constraints;
    Fmt.pf ppf "-------------------------\n@.")

let pprint_type_infer_value_verbose (ppf : Format.formatter) ~(verbose : bool)
    (value : Parsing.Parser_ast.value) (value_ty_attr : ty_attr)
    (value_constraints : constr list) : unit =
  if not verbose then ()
  else (
    Fmt.pf ppf "Actual value:@.";
    Parsing.Pprint_parser_ast.pprint_value ppf ~indent:"" value;
    Fmt.pf ppf "=> Value Ty:@.";
    pprint_ty_attr ppf value_ty_attr;
    Fmt.pf ppf "=> Value Constraints:@.";
    pprint_constraints ppf value_constraints;
    Fmt.pf ppf "-------------------------\n@.")

let pprint_substs (ppf : Format.formatter) (substs : subst list) : unit =
  List.iter substs ~f:(fun (ty_var, ty_subst) ->
      Fmt.pf ppf "Type Variable - %s, Type - %s@." ty_var
        (string_of_ty ty_subst))
