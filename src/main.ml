open Core;;

let source_code = "
  type custom_type = 
  | C of int

  
  fun add_y (x : custom_type option) (y : int) : int = begin
    match x with
    | None -> begin (); y end
    | Some C (x) -> begin x + y end
    endmatch
  end
  
  /*
  begin
    add_y (Some C(2), 5) < 10
  end */
" in
let parsed_program =
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_string source_code)
in
match Typing.Typecheck_program.typecheck_program parsed_program with
| Error err -> print_string (Error.to_string_hum err)
| Ok (typed_program) -> Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program

