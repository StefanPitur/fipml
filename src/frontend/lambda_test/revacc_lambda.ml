open Lambda

type 'a mlist = Nil | Cons of { mutable hd : 'a; mutable tl : 'a mlist }

let rev_acc_lambda =
  let reverse_acc = Ident.create_local "reverse_acc"
  and xs = Ident.create_local "xs"
  and tl = Ident.create_local "tl"
  and acc = Ident.create_local "acc" in
  let body =
    lfunction ~kind:Curried
      ~params:[ (xs, Pgenval); (acc, Pgenval) ]
      ~return:Pgenval ~loc:Loc_unknown ~attr:default_function_attribute
      ~body:
        (Lifthenelse
           ( Lvar xs,
             Llet
               ( Strict,
                 Pgenval,
                 tl,
                 Lprim (Pfield (1, Pointer, Mutable), [ Lvar xs ], Loc_unknown),
                 Lsequence
                   ( Lprim
                       ( Psetfield (1, Pointer, Assignment),
                         [ Lvar xs; Lvar acc ],
                         Loc_unknown ),
                     Lapply
                       {
                         ap_func = Lvar reverse_acc;
                         ap_args = [ Lvar tl; Lvar xs ];
                         ap_loc = Loc_unknown;
                         ap_tailcall = Default_tailcall;
                         ap_inlined = Default_inline;
                         ap_specialised = Default_specialise;
                       } ) ),
             Lvar acc ))
  in
  Lprim
    ( Psetglobal (Ident.create_persistent "Revacc"),
      [
        Lletrec
          ( [ (reverse_acc, body) ],
            Lprim
              ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
                [ Lvar reverse_acc ],
                Loc_unknown ) );
      ],
      Loc_unknown )

let write_revacc () =
  let fd = open_out "revacc.cmo" in
  (* Turn the lambda code into a sequence of instructions *)
  let instructions = Bytegen.compile_implementation "Revacc" rev_acc_lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Revacc" "revacc.cmo" ~required_globals:Ident.Set.empty
    instructions;
  close_out fd
