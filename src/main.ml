open Core;;

let channel = In_channel.create "src/main.fipml" in
let parsed_program =
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_channel channel)
in
match Typing.Typecheck_program.typecheck_program parsed_program with
| Error err -> print_string (Error.to_string_hum err)
| Ok typed_program ->
    Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program
