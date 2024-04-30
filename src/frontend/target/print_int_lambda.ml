open Fmt
open Lambda

let stdout_lambda =
  Lprim
    ( Pccall
        (Primitive.simple ~name:"caml_ml_open_descriptor_out" ~arity:1
           ~alloc:false),
      [ Lconst (const_int 1) ],
      Loc_unknown )

let my_string_of_int_ident = Ident.create_local "my_string_of_int"
let my_string_of_int_arg = Ident.create_local "x"

let my_string_of_int_lambda =
  lfunction ~kind:Curried
    ~params:[ (my_string_of_int_arg, Pintval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall (Primitive.simple ~name:"caml_format_int" ~arity:2 ~alloc:true),
           [ Lconst (Const_immstring "%d"); Lvar my_string_of_int_arg ],
           Loc_unknown ))

let my_string_length_ident = Ident.create_local "my_string_length"
let my_string_length_arg = Ident.create_local "s"

let my_string_length_lambda =
  lfunction ~kind:Curried
    ~params:[ (my_string_length_arg, Pgenval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall
             (Primitive.simple ~name:"caml_ml_string_length" ~arity:1
                ~alloc:false),
           [ Lvar my_string_length_arg ],
           Loc_unknown ))

let my_print_string_ident = Ident.create_local "my_print_string"
let my_print_string_arg = Ident.create_local "s"

let my_print_string_lambda =
  lfunction ~kind:Curried
    ~params:[ (my_print_string_arg, Pgenval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall (Primitive.simple ~name:"caml_ml_output" ~arity:4 ~alloc:false),
           [
             stdout_lambda;
             Lvar my_print_string_arg;
             Lconst (const_int 0);
             Lapply
               {
                 ap_func = Lvar my_string_length_ident;
                 ap_args = [ Lvar my_print_string_arg ];
                 ap_loc = Loc_unknown;
                 ap_tailcall = Default_tailcall;
                 ap_inlined = Default_inline;
                 ap_specialised = Default_specialise;
               };
           ],
           Loc_unknown ))

let my_print_int_ident = Ident.create_local "my_print_int"
let my_print_int_arg = Ident.create_local "x"
let aux_s = Ident.create_local "s"

let my_print_int_lambda =
  lfunction ~kind:Curried
    ~params:[ (my_print_int_arg, Pintval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Llet
         ( Strict,
           Pgenval,
           aux_s,
           Lapply
             {
               ap_func = Lvar my_string_of_int_ident;
               ap_args = [ Lvar my_print_int_arg ];
               ap_loc = Loc_unknown;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             },
           Lapply
             {
               ap_func = Lvar my_print_string_ident;
               ap_args = [ Lvar aux_s ];
               ap_loc = Loc_unknown;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             } ))

let program_lambda =
  Lprim
    ( Psetglobal (Ident.create_persistent "Print_int"),
      [
        Lletrec
          ( [
              (my_string_of_int_ident, my_string_of_int_lambda);
              (my_string_length_ident, my_string_length_lambda);
              (my_print_string_ident, my_print_string_lambda);
              (my_print_int_ident, my_print_int_lambda);
            ],
            Lsequence
              ( Lapply
                  {
                    ap_func = Lvar my_print_int_ident;
                    ap_args = [ Lconst (const_int 1923000) ];
                    ap_loc = Loc_unknown;
                    ap_tailcall = Default_tailcall;
                    ap_inlined = Default_inline;
                    ap_specialised = Default_specialise;
                  },
                Lprim
                  ( Pmakeblock
                      (0, Immutable, Some [ Pgenval; Pgenval; Pgenval; Pgenval ]),
                    [
                      Lvar my_string_of_int_ident;
                      Lvar my_string_length_ident;
                      Lvar my_print_string_ident;
                      Lvar my_print_int_ident;
                    ],
                    Loc_unknown ) ) );
      ],
      Loc_unknown )

let write_revacc () =
  let fd = open_out "print_int.cmo" in

  (*
     Running .cmo by
     ocamlc mod.cmo -o mod.byte
     ./mod.byte
  *)

  (* Turn the lambda code into a sequence of instructions *)
  (* We need both .cmx and .o *)
  (* let instructions = Asmgen.compile_implementation "Revacc" rev_acc_lambda in *)
  Printlambda.lambda Fmt.stdout program_lambda;
  let instructions =
    Bytegen.compile_implementation "Print_int" program_lambda
  in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Print_int" "print_int.cmo"
    ~required_globals:Ident.Set.empty instructions;
  close_out fd
