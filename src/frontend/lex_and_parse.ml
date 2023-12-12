open Lexing
open Core

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "File: %s, Line: %d, Column: %d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_source_code_with_error (lexbuf : lexbuf) = 
  try Parser.program Lexer.token lexbuf with
  | Lexer.LexerError msg -> 
    fprintf stderr "%a: %s\n" print_position lexbuf msg
  | Parser.Error ->
    fprintf stderr "%a: Syntax Error\n" print_position lexbuf;
    exit(-1)
