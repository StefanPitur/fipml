open Core

exception LexerException of string
exception ParserException of string

let parse_source_code_with_error (lexbuf : Lexing.lexbuf) :
    Parser_ast.program Or_error.t =
  try Ok (Parser.program Lexer.token lexbuf) with
  | Lexer.LexerError lexer_error ->
      let lexer_error_string =
        lexer_error ^ " - " ^ Ast.Ast_types.string_of_loc lexbuf.lex_curr_p
      in
      Or_error.of_exn (LexerException lexer_error_string)
  | Parser.Error ->
      let parsing_error_string =
        "Syntax Error - " ^ Ast.Ast_types.string_of_loc lexbuf.lex_curr_p
      in
      Or_error.of_exn (ParserException parsing_error_string)
