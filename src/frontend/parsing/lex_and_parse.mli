open Core

val parse_source_code_with_error :
  Lexing.lexbuf -> Parser_ast.program Or_error.t
(** Entry point for calling the lexer and parser on the initial source code *)
