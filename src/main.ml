let source_code = "\nbegin variable_name end\n" in
let program =
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_string source_code)
in
Parsing.Pprint_parser_ast.pprint_program Fmt.stdout program
