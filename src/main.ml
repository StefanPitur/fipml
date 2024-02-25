open Core;;

let source_code =
  "\n\
  \  type custom_type = \n\
  \  | C of int\n\n\
  \  \n\
  \  fun add_y (x : custom_type option) (y : int) : bool = begin\n\
  \    match x with\n\
  \    | None -> begin (); y end\n\
  \    | Some C (x) -> begin x + y end\n\
  \    endmatch\n\
  \  end\n\
  \  \n\
  \  /*\n\
  \  begin\n\
  \    add_y (Some C(2), 5) < 10\n\
  \  end */\n"
in
let parsed_program =
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_string source_code)
in
match Typing.Typecheck_program.typecheck_program parsed_program with
| Error err -> print_string (Error.to_string_hum err)
| Ok typed_program ->
    Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program
