let%expect_test "ignore_single_line_comment" =
  let source_code = "\n    // This is a single line comment, to be ignored\n  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {| Program |}]
;;
