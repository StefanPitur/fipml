let%expect_test "ignore_single_line_comment" =
  let source_code = "\n    // ignore this single-line comment\n  " in
  let lexbuf = Lexing.from_string source_code in
  Parsing.Pprint_lexer_token.pprint_lexer_token Fmt.stdout (Parsing.Lexer.token lexbuf);
  [%expect {| eof |}]
;;
