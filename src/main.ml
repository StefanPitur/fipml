open Ast.Ast_types
open Core;;

let channel = In_channel.create "src/binary-tree-traversal.fipml" in
match
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_channel channel)
with
| Ok parsed_program -> (
    match Typing.Typecheck_program.typecheck_program parsed_program with
    | Error err -> print_string (Error.to_string_hum err)
    | Ok (typed_program, functions_env) ->
        let (TProg (_, _, typed_function_defns, _)) = typed_program in
        List.iter typed_function_defns ~f:(fun typed_function_defn ->
            let (TFun (_, _, _, _, typed_function_name, _, _)) =
              typed_function_defn
            in
            let fiped_function_defn =
              Or_error.ok_exn
                (Fip.Static_fip.fip typed_function_defn functions_env)
            in
            Fmt.pf Fmt.stdout
              "<><><><><><><><><><><><><><><><><><><>\n\nFunction Name - %s@."
              (Function_name.to_string typed_function_name);
            Fip.Pprint_fip_ast.pprint_fip_expr Fmt.stdout ~indent:""
              fiped_function_defn))
| Error err -> print_string (Error.to_string_hum err)
