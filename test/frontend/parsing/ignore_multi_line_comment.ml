let%expect_test "ignore_multi_line_comment" =
  let source_code =
    "\n\
    \    /*\n\
    \      This is a multi-line comment\n\
    \      and should be ignored regardless of what \n\
    \      is 1 () match inside\n\
    \    */\n\
    \  "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect {| Program |}]
;;
