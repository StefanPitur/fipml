open Lambda

let stdout_lambda =
  Lprim
    ( Pccall
        (Primitive.simple ~name:"caml_ml_open_descriptor_out" ~arity:1
           ~alloc:false),
      [ Lconst (const_int 1) ],
      Loc_unknown )

let _string_of_int_ident = Ident.create_local "_string_of_int"
let _string_of_int_arg = Ident.create_local "x"

let _string_of_int_lambda =
  lfunction ~kind:Curried
    ~params:[ (_string_of_int_arg, Pintval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall (Primitive.simple ~name:"caml_format_int" ~arity:2 ~alloc:true),
           [ Lconst (Const_immstring "%d"); Lvar _string_of_int_arg ],
           Loc_unknown ))

let _string_length_ident = Ident.create_local "_string_length"
let _string_length_arg = Ident.create_local "s"

let _string_length_lambda =
  lfunction ~kind:Curried
    ~params:[ (_string_length_arg, Pgenval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall
             (Primitive.simple ~name:"caml_ml_string_length" ~arity:1
                ~alloc:false),
           [ Lvar _string_length_arg ],
           Loc_unknown ))

let _print_string_ident = Ident.create_local "_print_string"
let _print_string_arg = Ident.create_local "s"

let _print_string_lambda =
  lfunction ~kind:Curried
    ~params:[ (_print_string_arg, Pgenval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Lprim
         ( Pccall (Primitive.simple ~name:"caml_ml_output" ~arity:4 ~alloc:false),
           [
             stdout_lambda;
             Lvar _print_string_arg;
             Lconst (const_int 0);
             Lapply
               {
                 ap_func = Lvar _string_length_ident;
                 ap_args = [ Lvar _print_string_arg ];
                 ap_loc = Loc_unknown;
                 ap_tailcall = Default_tailcall;
                 ap_inlined = Default_inline;
                 ap_specialised = Default_specialise;
               };
           ],
           Loc_unknown ))

let _print_int_ident = Ident.create_local "_print_int"
let _print_int_arg = Ident.create_local "x"
let s = Ident.create_local "s"

let _print_int_lambda =
  lfunction ~kind:Curried
    ~params:[ (_print_int_arg, Pintval) ]
    ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
    ~body:
      (Llet
         ( Strict,
           Pgenval,
           s,
           Lapply
             {
               ap_func = Lvar _string_of_int_ident;
               ap_args = [ Lvar _print_int_arg ];
               ap_loc = Loc_unknown;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             },
           Lapply
             {
               ap_func = Lvar _print_string_ident;
               ap_args = [ Lvar s ];
               ap_loc = Loc_unknown;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             } ))

let import_print_int_lambda_letrecs () : (Ident.t * lambda) list =
  [
    (_string_of_int_ident, _string_of_int_lambda);
    (_string_length_ident, _string_length_lambda);
    (_print_string_ident, _print_string_lambda);
    (_print_int_ident, _print_int_lambda);
  ]
