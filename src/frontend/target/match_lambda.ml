open Lambda

let match_lambda_f = Ident.create_local "match"

let match_lambda =
  let match_var = Ident.create_local "x" in
  lfunction ~kind:Curried
    ~params:[ (match_var, Pgenval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lstaticcatch
         ( Lswitch
             ( Lvar match_var,
               {
                 sw_numconsts = 0;
                 sw_consts = [];
                 sw_numblocks = 2;
                 sw_blocks = [ (1, Lvar match_var) ];
                 sw_failaction = Some (Lstaticraise (0, []));
               },
               Loc_unknown ),
           (0, []),
           lambda_unit ))
(* Lprim
   ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
     [ lambda_unit ],
     Loc_unknown ) )) *)

let program_lambda =
  let main = Ident.create_local "main" in
  let x = Ident.create_local "x" in
  let main_function =
    lfunction ~kind:Curried
      ~params:[ (x, Pgenval) ]
      ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
      ~body:
        (Lapply
           {
             ap_func = Lvar match_lambda_f;
             ap_args = [ Lvar x ];
             ap_loc = Loc_unknown;
             ap_tailcall = Default_tailcall;
             ap_inlined = Default_inline;
             ap_specialised = Default_specialise;
           })
  in
  Lprim
    ( Psetglobal (Ident.create_persistent "My_match"),
      [
        Lletrec
          ( [ (main, main_function); (match_lambda_f, match_lambda) ],
            Lprim
              ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
                [ Lvar main ],
                Loc_unknown ) );
      ],
      Loc_unknown )

let write_revacc () =
  let fd = open_out "my_match.cmo" in

  (*
     Running .cmo by
     ocamlc mod.cmo -o mod.byte
     ./mod.byte
  *)

  (* Turn the lambda code into a sequence of instructions *)
  (* We need both .cmx and .o *)
  (* let instructions = Asmgen.compile_implementation "Revacc" rev_acc_lambda in *)
  Printlambda.lambda Fmt.stdout program_lambda;
  let instructions = Bytegen.compile_implementation "My_match" program_lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "My_match" "my_match.cmo"
    ~required_globals:Ident.Set.empty instructions;
  close_out fd
