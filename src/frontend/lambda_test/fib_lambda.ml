open Lambda
let fib_lambda =
  let fib = Ident.create_local "fib"
  and n = Ident.create_local "n" in
  let body =
    lfunction
      ~kind:Curried
      ~params:[n, Pgenval]
      ~return:Pgenval
      ~loc:Loc_unknown
      ~attr:default_function_attribute
      ~body:
        (Lprim (Pintcomp Clt, [Lconst (Const_base (Const_int 0)); Lvar n], Loc_unknown))
  in
  Lprim
    (Psetglobal (Ident.create_persistent "Fib"),
     [Lletrec ([fib, body], Lprim (Pmakeblock (0, Immutable, Some [Pgenval]), [Lvar fib], Loc_unknown))], Loc_unknown)

let write_fib () =
  let fd = open_out "fib.cmo" in
  (* Turn the lambda code into a sequence of instructions *)
  let instructions = Bytegen.compile_implementation "Fib" fib_lambda in
  (* Write the sequence of instructions to the file revacc.cmo *)
  Emitcode.to_file fd "Fib" "fib.cmo" ~required_globals:Ident.Set.empty instructions;
  close_out fd
