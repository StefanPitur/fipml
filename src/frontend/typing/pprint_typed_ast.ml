open Ast.Ast_types
open Typed_ast

let indent_tab = "    "

(* Pretty-printing Type Definition *)
let rec pprint_type_defn ppf ~indent
    (TType (_, _, type_name, type_constructors)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Name: %s@." indent (Type_name.to_string type_name);
  Fmt.pf ppf "%sType Constructors:@." indent;
  List.iter
    (pprint_type_constructor ppf ~indent:sub_expr_indent)
    type_constructors

and pprint_type_constructor ppf ~indent
    (TTypeConstructor (_, _, constructor_name, type_exprs)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Constructor Name: %s@." indent
    (Constructor_name.to_string constructor_name);
  List.iter
    (Ast.Pprint_ast_types.pprint_type_expr ppf ~indent:sub_expr_indent)
    type_exprs

(* Pretty-printing Program *)
and pprint_typed_program ppf (TProg (type_defns, _, _, _)) =
  Fmt.pf ppf "Typed Program@.";
  List.iter (pprint_type_defn ppf ~indent:indent_tab) type_defns
