open Core

let pprint_parser_ast source_code =
  match
    Parsing.Lex_and_parse.parse_source_code_with_error
      (Lexing.from_string source_code)
  with
  | Ok program -> Parsing.Pprint_parser_ast.pprint_program Fmt.stdout program
  | Error error -> print_string (Error.to_string_hum error)
