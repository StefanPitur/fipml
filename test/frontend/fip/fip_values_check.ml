(* open Ast.Ast_types
   open Core
   open Typing.Fip_rules_check
   open Typing.Borrowed_context
   open Typing.Pprint_fip_ast
   open Typing

   let mock_loc : Lexing.position =
     { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

   let%expect_test "FIP rules for values : Unit" =
     let value_unit = Typed_ast.Unit (mock_loc, TEUnit mock_loc) in
     let fip_value =
       Or_error.ok_exn (fip_rules_check_value value_unit BorrowedSet.empty)
     in
     pprint_fip_value Fmt.stdout ~indent:"" fip_value;
     [%expect
       {|
       Borrowed - []
       Owned - []
       Reuse

       Fip Value: Unit |}]

   let%expect_test "FIP rules for values : Integer/Boolean" =
     let value_integer = Typed_ast.Integer (mock_loc, TEInt mock_loc, 0) in
     let fip_value =
       Or_error.ok_exn (fip_rules_check_value value_integer BorrowedSet.empty)
     in
     pprint_fip_value Fmt.stdout ~indent:"" fip_value;
     [%expect
       {|
       Borrowed - []
       Owned - []
       Reuse

       Fip Value: Integer - 0 |}]

   let%expect_test "FIP rules for values : Variable" =
     let value_var =
       Typed_ast.Variable (mock_loc, TEBool mock_loc, Var_name.of_string "x")
     in
     let fip_value =
       Or_error.ok_exn (fip_rules_check_value value_var BorrowedSet.empty)
     in
     pprint_fip_value Fmt.stdout ~indent:"" fip_value;
     [%expect
       {|
       Borrowed - []
       Owned - [x]
       Reuse

       Fip Value: Variable - x |}]

   let%expect_test "FIP rules for values : Atom" =
     let value_atom =
       Typed_ast.Constructor
         ( mock_loc,
           TECustom (mock_loc, [], Type_name.of_string "custom_type"),
           Constructor_name.of_string "Atom",
           [] )
     in
     let fip_value =
       Or_error.ok_exn (fip_rules_check_value value_atom BorrowedSet.empty)
     in
     pprint_fip_value Fmt.stdout ~indent:"" fip_value;
     [%expect
       {|
       Borrowed - []
       Owned - []
       Reuse

       Fip Value: Constructor - Atom |}]

   let%expect_test "FIP rules for values : Complex Constructor" =
     let value_complex =
       Typed_ast.Constructor
         ( mock_loc,
           TECustom (mock_loc, [], Type_name.of_string "custom_type"),
           Constructor_name.of_string "ComplexConstructor",
           [
             Typed_ast.Boolean (mock_loc, TEBool mock_loc, true);
             Typed_ast.Constructor
               ( mock_loc,
                 TECustom (mock_loc, [], Type_name.of_string "custom_type"),
                 Constructor_name.of_string "Atom",
                 [] );
             Typed_ast.Variable (mock_loc, TEInt mock_loc, Var_name.of_string "x");
           ] )
     in
     let fip_value =
       Or_error.ok_exn (fip_rules_check_value value_complex BorrowedSet.empty)
     in
     pprint_fip_value Fmt.stdout ~indent:"" fip_value;
     [%expect
       {|
       Borrowed - []
       Owned - [x]
       Reuse
       Key = 3 :: Count = 1 - [_todo]
       Fip Value: Constructor - ComplexConstructor
           Borrowed - []
           Owned - []
           Reuse

           Fip Value: Boolean - true
           Borrowed - []
           Owned - []
           Reuse

           Fip Value: Constructor - Atom
           Borrowed - []
           Owned - [x]
           Reuse

           Fip Value: Variable - x |}] *)
