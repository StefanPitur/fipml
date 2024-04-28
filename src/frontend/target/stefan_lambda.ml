open Lambda

let basic_lambda_code =
  let main = Ident.create_local "main" in
  let body =
    Lprim (Paddint, [ Lconst (const_int 1); Lconst (const_int 2) ], Loc_unknown)
  in
  Lprim
    ( Psetglobal (Ident.create_persistent "Stefan"),
      [
        Llet
          ( Strict,
            Pgenval,
            main,
            body,
            Lprim
              ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
                [ Lvar main ],
                Loc_unknown ) );
      ],
      Loc_unknown )

(* open Lambda

   let stefan_lambda =
     let stefan = Ident.create_local "stefan"
     and stefan2 = Ident.create_local "stefan2"
     and x = Ident.create_local "x"
     and y = Ident.create_local "y" in
     let stefan_body =
       lfunction ~kind:Curried
         ~params:[ (x, Pgenval); (y, Pgenval) ]
         ~return:Pgenval ~loc:Loc_unknown ~attr:default_function_attribute
         ~body:(Lprim (Paddint, [ Lvar x; Lvar y ], Loc_unknown))
     in
     let stefan2_body =
       lfunction ~kind:Curried
         ~params:[ (x, Pgenval); (y, Pgenval) ]
         ~return:Pgenval ~loc:Loc_unknown ~attr:default_function_attribute
         ~body:(Lprim (Pmulint, [ Lvar x; Lvar y ], Loc_unknown))
     in
     Lprim
       ( Psetglobal (Ident.create_persistent "Stefan"),
         [
           Lletrec
             ( [ (stefan, stefan_body); (stefan2, stefan2_body) ],
               Lprim
                 ( Pmakeblock (0, Mutable, Some [ Pgenval; Pgenval ]),
                   [ Lvar stefan; Lvar stefan2 ],
                   Loc_unknown ) );
         ],
         Loc_unknown )
*)
let write_revacc () =
  let fd = open_out "stefan.cmo" in
  (* Turn the lambda code into a sequence of instructions *)
  let instructions =
    Bytegen.compile_implementation "Stefan" basic_lambda_code
  in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Stefan" "stefan.cmo" ~required_globals:Ident.Set.empty
    instructions;
  close_out fd

(* let print_stefan_lambda = Printlambda.lambda Fmt.stdout stefan_lambda *)
