open Core
open Result

let write_fip (program_lambda : Lambda.lambda) =
  let fd = Out_channel.create "src/experiments/msort/fip.cmo" in

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
  Emitcode.to_file fd "Fip" "src/experiments/msort/fip.cmo"
    ~required_globals:Ident.Set.empty instructions;
  Out_channel.close fd
;;

let channel = In_channel.create "src/experiments/msort/msort.fipml" in
Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_channel channel)
>>= fun parsed_program ->
Typing.Typecheck_program.typecheck_program parsed_program
>>= fun (typed_program, _, types_env, constructors_env, fip_function_defns) ->
let constructor_tag_map =
  Target.Pre_lambda.compute_custom_constructors_tags types_env constructors_env
in
Map.iteri constructor_tag_map ~f:(fun ~key ~data ->
    Fmt.pf Fmt.stdout "%s - %d@."
      (Ast.Ast_types.Constructor_name.to_string key)
      data);
Fmt.pf Fmt.stdout "\n";
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
(* Fmt.pf Fmt.stdout "5"; *)
Target.Convert_pre_lambda_to_lambda.target_program pre_lambda_program
  constructor_tag_map
>>= fun lambda_program -> Ok (write_fip lambda_program)
