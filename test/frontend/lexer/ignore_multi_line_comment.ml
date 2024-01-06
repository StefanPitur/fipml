let%expect_test "ignore_multi_line_comment" = 
  let source_code = "
    /*
      This is a multi-line comment
      and should be ignored regardless of what 
      is 1 () match inside
    */
  " in
  let lexbuf = Lexing.from_string source_code in
  Parsing.Pprint_lexer_token.pprint_lexer_token Fmt.stdout (Parsing.Lexer.token lexbuf);
  [%expect {| eof |}]