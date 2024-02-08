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
  let lexbuf = Lexing.from_string source_code in
  Parsing.Pprint_lexer_token.pprint_lexer_token Fmt.stdout (Parsing.Lexer.token lexbuf);
  [%expect {| eof |}]
;;
