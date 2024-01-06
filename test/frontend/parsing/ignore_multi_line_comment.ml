let%expect_test "ignore_multi_line_comment" = 
  let source_code = "
    /*
      This is a multi-line comment
      and should be ignored regardless of what 
      is 1 () match inside
    */
  " in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {| Program |}]