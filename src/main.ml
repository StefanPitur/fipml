open Core;;

(* let mock_loc : Lexing.position =
     { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
   ;; *)

let channel = In_channel.create "src/main.fipml" in
match
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_channel channel)
with
| Ok parsed_program -> (
    (* Parsing.Pprint_parser_ast.pprint_program Fmt.stdout parsed_program *)
    match Typing.Typecheck_program.typecheck_program parsed_program with
    | Error err -> print_string (Error.to_string_hum err)
    | Ok typed_program ->
        Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program
        (*let (Typing.Typed_ast.TProg (_, _, function_defns, _)) =
            typed_program
          in
          let functions_env =
            [
              Typing.Functions_env.FunctionEnvEntry
                ( Some (Ast.Ast_types.Fip 0),
                  Ast.Ast_types.Function_name.of_string "example2_5",
                  [
                    Ast.Ast_types.TECustom
                      (mock_loc, Ast.Ast_types.Type_name.of_string "boolean");
                  ],
                  Ast.Ast_types.TECustom
                    (mock_loc, Ast.Ast_types.Type_name.of_string "list") );
            ]
          in
          match function_defns with
          | [] -> ()
          | function_defn :: _ ->
              let fip_expr =
                Or_error.ok_exn (Fbip.fbip function_defn functions_env)
              in
              Pprint_fip_ast.pprint_fip_expr Fmt.stdout ~indent:"" fip_expr) *))
| Error err -> print_string (Error.to_string_hum err)
