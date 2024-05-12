open Ast_types

let pprint_borrowed ppf ~indent borrowed =
  Fmt.pf ppf "%sBorrowed: %s@." indent
    (string_of_borrowed_option (Some borrowed))

let pprint_type_expr ppf ~indent type_expr =
  Fmt.pf ppf "%sType Expr: %s@." indent (string_of_type type_expr)

let pprint_param ppf ~indent = function
  | TParam (type_expr, param_name, param_borrowed) ->
      pprint_type_expr ppf ~indent type_expr;
      Fmt.pf ppf "%s%sParam: %s@." indent
        (string_of_borrowed_option param_borrowed)
        (Var_name.to_string param_name)

let pprint_params ppf ~indent = function
  | [] -> Fmt.pf ppf "%sVoid@." indent
  | params -> List.iter (pprint_param ppf ~indent) params

let pprint_unary_op ppf ~indent unary_op =
  Fmt.pf ppf "%sUnary Op: %s@." indent (string_of_unary_op unary_op)

let pprint_binary_op ppf ~indent binary_op =
  Fmt.pf ppf "%sBinary Op: %s@." indent (string_of_binary_op binary_op)
