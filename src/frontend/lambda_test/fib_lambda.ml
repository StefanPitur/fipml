open Lambda

let fib_lambda =
  let fib = Ident.create_local "fib"
  and n = Ident.create_local "n"
  and acc = Ident.create_local "acc" in
  let body =
    lfunction ~kind:Curried
      ~params:[ (n, Pgenval); (acc, Pgenval) ]
      ~return:Pgenval ~loc:Loc_unknown ~attr:default_function_attribute
      ~body:
        (Lifthenelse
           ( Lprim
               ( Pintcomp Cle,
                 [ Lvar n; Lconst (Const_base (Const_int 0)) ],
                 Loc_unknown ),
             Lvar acc,
             Llet
               ( Alias,
                 Pgenval,
                 acc,
                 Lprim (Paddint, [ Lvar n; Lvar acc ], Loc_unknown),
                 Llet
                   ( Alias,
                     Pgenval,
                     n,
                     Lprim
                       ( Psubint,
                         [ Lvar n; Lconst (Const_base (Const_int 1)) ],
                         Loc_unknown ),
                     Lapply
                       {
                         ap_func = Lvar fib;
                         ap_args = [ Lvar n; Lvar acc ];
                         ap_loc = Loc_unknown;
                         ap_tailcall = Default_tailcall;
                         ap_inlined = Default_inline;
                         ap_specialised = Default_specialise;
                       } ) ) ))
  in
  Lprim
    ( Psetglobal (Ident.create_persistent "Fib"),
      [
        Lletrec
          ( [ (fib, body) ],
            Lprim
              ( Pmakeblock (0, Immutable, Some [ Pgenval ]),
                [ Lvar fib ],
                Loc_unknown ) );
      ],
      Loc_unknown )

let write_fib () =
  let fd = open_out "fib.cmo" in
  (* Turn the lambda code into a sequence of instructions *)
  let instructions = Bytegen.compile_implementation "Fib" fib_lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Fib" "fib.cmo" ~required_globals:Ident.Set.empty
    instructions;
  close_out fd
