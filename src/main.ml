open Core
open Lambda
open Result

(* let write_fip (program_lambda : lambda) : unit =
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
   ;; *)

let write_fip_opt (program_lambda : lambda) (program_modname : Ident.t) : unit =
  Printlambda.lambda Fmt.stdout program_lambda;
  let module Backend = struct
    let symbol_for_global' = Compilenv.symbol_for_global'
    let closure_symbol = Compilenv.closure_symbol
    let really_import_approx = Import_approx.really_import_approx
    let import_symbol = Import_approx.import_symbol
    let size_int = Arch.size_int
    let big_endian = Arch.big_endian
    let max_sensible_number_of_arguments = Proc.max_arguments_for_tailcalls - 1
  end in
  let backend = (module Backend : Backend_intf.S) in
  let fip_program =
    {
      module_ident = program_modname;
      main_module_block_size = 24;
      required_globals = Ident.Set.(add program_modname empty);
      code = program_lambda;
    }
  in
  ignore (Env.read_signature "Fip" "src/experiments/msort/fip.cmi");
  Env.set_unit_name "Fip";
  let () = Compilenv.reset "Fip" in
  Asmgen.compile_implementation ~backend ~prefixname:"src/experiments/msort/fip"
    ~middle_end:Closure_middle_end.lambda_to_clambda
    ~ppf_dump:Format.std_formatter fip_program;
  Compilenv.save_unit_info "src/experiments/msort/fip.cmx"
;;

(* "qualitative evaluation as a programming language - Chapter 4" *)
let channel = In_channel.create "src/experiments/msort/msort.fipml" in
Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_channel channel)
>>= fun parsed_program ->
Fmt.pf Fmt.stdout "1\n";
Typing.Typecheck_program.typecheck_program parsed_program
>>= fun (typed_program, _, types_env, constructors_env, fip_function_defns) ->
Fmt.pf Fmt.stdout "2\n";
let constructor_tag_map =
  Target.Pre_lambda.compute_custom_constructors_tags types_env constructors_env
in
Fmt.pf Fmt.stdout "3\n";
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
Fmt.pf Fmt.stdout "3\n";
let pre_lambda_program =
  Target.Pre_lambda.TProg
    ( constructor_tag_map,
      function_defn @ pre_lambda_fip_function_defns,
      main_expr_option )
in
let program_modname = Ident.create_persistent "Fip" in
Target.Convert_pre_lambda_to_lambda.target_program pre_lambda_program
  constructor_tag_map program_modname
>>= fun lambda_program -> Ok (write_fip_opt lambda_program program_modname)
