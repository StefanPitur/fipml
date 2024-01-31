open Core
open Pprint_lexbuf_position


let parse_source_code_with_error (lexbuf : Lexing.lexbuf) = 
  try Parser.program Lexer.token lexbuf with
  | Lexer.LexerError lexer_error -> 
    Fmt.pf Fmt.stderr "%s@." lexer_error;
    pprint_lexbuf_position Fmt.stderr lexbuf;
    exit(-1)
  | Parser.Error ->
    Fmt.pf Fmt.stderr "Syntax Error@.";
    pprint_lexbuf_position Fmt.stderr lexbuf;
    exit(-1)
