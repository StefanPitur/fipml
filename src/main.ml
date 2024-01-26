(* let source_code = "
  type my_first_type = 
    | C1
    | C2 of bool * int * my_second_type

  // some comment to be ignored
  /*Multi-line
    comment
  everywhere*/

  fun f4 (x : int) (^y : my_first_type) : unit = begin
    ()
  end

  fun rec f5 (x1 : type1) (^y1 : type2) (x2 : int) (^y2 : int option) : unit = begin
    (); fst (x1 + x2, ());
    match x1 with
    | x -> begin () end
    | y -> begin () end
    | C(x) -> begin () end
    | (x, y) -> begin true end
    | Some (x, y) -> begin true end
    | None -> begin false end
    | _ -> begin false end
    | Some _ -> begin true end
    endmatch
  end

  /*let x = Some (x, y) in
  let y = None in 
  fn1 ((1, 2), x, y, true, ())*/
  match x with
  | C -> begin () end
  | x -> begin x; () end
  | _ -> begin () end
  endmatch
" *)
let source_code = "
  type my_type = 
  | Constructor1 of int -> int
  | Constructor2 of (int -> int) option
  | Constructor3 of int -> int option
"
in
let program = Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_string source_code) in
Parsing.Pprint_parser_ast.pprint_program Fmt.stdout program