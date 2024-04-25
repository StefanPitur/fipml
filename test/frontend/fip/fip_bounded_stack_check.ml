open Ast.Ast_types
open Core
open Typing.Typed_ast

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let mock_type_expr : type_expr =
  TAttr (mock_loc, TEUnit mock_loc, Shared mock_loc)

let%expect_test "Free Variables" =
  let expr =
    Let
      ( mock_loc,
        mock_type_expr,
        [ mock_type_expr ],
        [ Var_name.of_string "x" ],
        UnboxedSingleton
          ( mock_loc,
            mock_type_expr,
            Variable (mock_loc, mock_type_expr, Var_name.of_string "x") ),
        UnboxedSingleton
          ( mock_loc,
            mock_type_expr,
            Variable (mock_loc, mock_type_expr, Var_name.of_string "y") ) )
  in
  let free_variables_set =
    Parsing.Parser_ast.free_variables (convert_typed_to_parser expr)
  in
  Set.iter free_variables_set ~f:(fun free_var ->
      Fmt.pf Fmt.stdout "%s " (Var_name.to_string free_var));
  [%expect {| y |}]
