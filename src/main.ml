open Core
open Result

let write_fip (program_lambda : Lambda.lambda) =
  let fd = Out_channel.create "src/fip.cmo" in

  (*
     Running .cmo by
     ocamlc mod.cmo -o mod.byte
     ./mod.byte
  *)

  (* Turn the lambda code into a sequence of instructions *)
  (* We need both .cmx and .o *)
  (* let instructions = Asmgen.compile_implementation "Revacc" rev_acc_lambda in *)
  Printlambda.lambda Fmt.stdout program_lambda;
  let instructions = Bytegen.compile_implementation "Fip" program_lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Fip" "src/fip.cmo" ~required_globals:Ident.Set.empty
    instructions;
  Out_channel.close fd
;;

let channel = In_channel.create "src/red-black-trees.fipml" in
Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_channel channel)
>>= fun parsed_program ->
Typing.Typecheck_program.typecheck_program parsed_program
>>= fun (typed_program, _, types_env, constructors_env, fip_function_defns) ->
let constructor_tag_map =
  Target.Pre_lambda.compute_custom_constructors_tags types_env constructors_env
in
let (Target.Pre_lambda.TProg (_, function_defn, main_expr_option)) =
  Target.Convert_typed_ast_to_pre_lambda.convert_typed_ast_to_pre_lambda_program
    constructors_env constructor_tag_map typed_program
in
let pre_lambda_fip_function_defns =
  Target.Convert_fip_ast_to_pre_lambda
  .convert_fip_ast_to_pre_lambda_function_defns constructors_env
    constructor_tag_map fip_function_defns
in
let pre_lambda_program =
  Target.Pre_lambda.TProg
    ( constructor_tag_map,
      function_defn @ pre_lambda_fip_function_defns,
      main_expr_option )
in
Target.Pprint_pre_lambda.pprint_pre_lambda_program Fmt.stdout pre_lambda_program;
Target.Convert_pre_lambda_to_lambda.target_program pre_lambda_program
  constructor_tag_map
>>= fun lambda_program -> Ok (write_fip lambda_program)

(*
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

let typed_function_defn =
  Typed_ast.TFun
    ( mock_loc,
      TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
      0,
      None,
      Function_name.of_string "some_function",
      [
        TParam
          ( TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
            Var_name.of_string "a",
            None );
      ],
      UnboxedSingleton
        ( mock_loc,
          TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
          Variable
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
              Var_name.of_string "a" ) ) )
in
let typed_main_expr =
  Typed_ast.Let
    ( mock_loc,
      TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
      [],
      [ Var_name.of_string "x1" ],
      FunCall
        ( mock_loc,
          TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
          Function_name.of_string "some_function",
          [
            Integer
              (mock_loc, TAttr (mock_loc, TEInt mock_loc, Shared mock_loc), -13);
          ] ),
      Typed_ast.UnOp
        ( mock_loc,
          TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
          UnOpNeg,
          Typed_ast.UnboxedSingleton
            ( mock_loc,
              TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
              Typed_ast.Variable
                ( mock_loc,
                  TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
                  Var_name.of_string "x1" ) ) ) )
in
match
  Convert_typed_ast_to_lambda.convert_typed_ast_program
    (Typed_ast.TProg
       ( [],
         TAttr (mock_loc, TEInt mock_loc, Shared mock_loc),
         [ typed_function_defn ],
         Some typed_main_expr ))
    [] []
with
| Error _ -> print_string "some error occurred"
| Ok program_lambda ->
    Printlambda.lambda Fmt.stdout program_lambda;
    write_lambda program_lambda
*)
