open Core

let parse_source_code_with_error (lexbuf : Lexing.lexbuf) =
  try Parser.program Lexer.token lexbuf with
  | Lexer.LexerError lexer_error ->
      Fmt.pf Fmt.stderr "%s@." lexer_error;
      Fmt.pf Fmt.stderr "%s@." (Ast.Ast_types.string_of_loc lexbuf.lex_curr_p);
      exit (-1)
  | Parser.Error ->
      Fmt.pf Fmt.stderr "Syntax Error@.";
      Fmt.pf Fmt.stderr "%s@." (Ast.Ast_types.string_of_loc lexbuf.lex_curr_p);
      exit (-1)
