(* open Ast.Ast_types
   open Core;;

   let channel = In_channel.create "src/red-black-trees.fipml" in
   match
     Parsing.Lex_and_parse.parse_source_code_with_error
       (Lexing.from_channel channel)
   with
   | Ok parsed_program -> (
       (* Parsing.Pprint_parser_ast.pprint_program Fmt.stdout parsed_program; *)
       match Typing.Typecheck_program.typecheck_program parsed_program with
       | Error err -> print_string (Error.to_string_hum err)
       | Ok (typed_program, functions_env, _) ->
           Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program;
           Fmt.pf Fmt.stdout "\n\n<><><><><>\n";
           Typing.Functions_env.pprint_functions_env Fmt.stdout functions_env
           (* Fmt.pf Fmt.stdout "\n\n<><><><><>\n";
              List.iter fiped_function_defns
                ~f:
                  (Typing.Pprint_fip_ast.pprint_fip_function_defn Fmt.stdout
                     ~indent:"")
                        fiped_function_defn)) *))
   | Error err -> print_string (Error.to_string_hum err) *)

open Ast.Ast_types
open Core
open Lambda
open Target
open Typing
open Result

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let write_lambda (lambda : lambda) : unit =
  let fd = Out_channel.create "src/fip.cmo" in
  (* Turn the lambda code into a sequence of instructions *)
  let instructions = Bytegen.compile_implementation "Fip" lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Fip" "src/fip.cmo" ~required_globals:Ident.Set.empty
    instructions;
  Out_channel.close fd
;;

let typed_function =
  Typed_ast.TFun
    ( mock_loc,
      TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
      0,
      None,
      Function_name.of_string "my_function",
      [
        TParam
          ( TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
            Var_name.of_string "x",
            None );
        TParam
          ( TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
            Var_name.of_string "y",
            None );
      ],
      Typed_ast.BinaryOp
        ( mock_loc,
          TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
          BinOpPlus,
          Typed_ast.UnboxedSingleton
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
              Typed_ast.Variable
                ( mock_loc,
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
                  Var_name.of_string "x" ) ),
          Typed_ast.UnboxedSingleton
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
              Typed_ast.Variable
                ( mock_loc,
                  TAttr (mock_loc, TEInt mock_loc, Unique mock_loc),
                  Var_name.of_string "y" ) ) ) )
in
let typed_main_expr =
  Typed_ast.FunCall
    ( mock_loc,
      TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
      Function_name.of_string "my_function",
      [
        Typed_ast.Integer
          (mock_loc, TAttr (mock_loc, TEInt mock_loc, Shared mock_loc), 2);
        Typed_ast.Integer
          (mock_loc, TAttr (mock_loc, TEInt mock_loc, Shared mock_loc), 40);
      ] )
in
match
  Convert_typed_ast_to_lambda.convert_typed_ast_program
    (Typed_ast.TProg
       ( [],
         TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
         [ typed_function ],
         Some typed_main_expr ))
    []
with
| Error _ -> print_string "some error occurred"
| Ok program_lambda ->
    Printlambda.lambda Fmt.stdout program_lambda;
    write_lambda program_lambda
