open Core
open Parsing.Parser_ast
open Typing

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let constructors_env : Type_defns_env.constructors_env = []
let functions_env : Functions_env.functions_env = []
let typing_context : Type_infer.typing_context = []

let%expect_test "Constraints Generation: Unit" =
  let expr = Unit mock_loc in
  let (typing_context, expr_type, constraints) = Or_error.ok_exn (Type_infer.generate_constraints constructors_env functions_env typing_context expr) in
  Pprint_type_infer.pprint_typing_context Fmt.stdout typing_context;
  [%expect {||}];
  Pprint_type_infer.pprint_ty Fmt.stdout expr_type;
  [%expect {| TyUnit |}];
  Pprint_type_infer.pprint_constraints Fmt.stdout constraints;
  [%expect {||}]
