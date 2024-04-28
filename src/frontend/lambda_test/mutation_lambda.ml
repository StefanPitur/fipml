open Lambda

(*
   let x = ref 0 in
   x := !x + 1;
   !x
*)

let mutation_lambda =
  (* let my_x = Ident.create_local "my_x" in
     let my_x_assingment = Lprim (Pmakeblock (0, Mutable, Some [Pgenval]), [Lconst (Const_base (Const_int 0))], Loc_unknown) in
     let code = Llet (Strict, Pgenval, my_x, my_x_assingment, lambda_unit) in
     Lprim (Psetglobal (Ident.create_persistent "MutationTest"), [code], Loc_unknown) *)
  let increase_ref = Ident.create_local "increase_ref" in
  (* and x = Ident.create_local "x" in *)
  let body =
    lfunction ~kind:Curried ~params:[] ~return:Pgenval ~loc:Loc_unknown
      ~attr:default_function_attribute
      ~body:(Lconst (Const_base (Const_int 42)))
    (* (Llet (Strict, Pgenval, x, Lprim (Pmakeblock (0, Mutable, Some [Pgenval]), [Lconst (Const_base (Const_int 0))], Loc_unknown), lambda_unit)) *)
  in
  Lprim
    ( Psetglobal (Ident.create_persistent "Mutation"),
      [
        Llet
          ( Strict,
            Pgenval,
            increase_ref,
            body,
            Lprim
              ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
                [ Lvar increase_ref ],
                Loc_unknown ) );
      ],
      Loc_unknown )

let write_mutation () =
  let fd = open_out "mutation.cmo" in
  let instructions =
    Bytegen.compile_implementation "Mutation" mutation_lambda
  in
  Emitcode.to_file fd "Mutation" "mutation.cmo"
    ~required_globals:Ident.Set.empty instructions;
  close_out fd
