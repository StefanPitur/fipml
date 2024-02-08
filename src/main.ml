(* open Core;;

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
   | Constructor1 of int -> int -> int
   | Constructor2 of int -> (int -> int -> int) -> int option
   | Constructor3 of int -> int option

   type custom_simple_type =
   | SimpleConstructor1

   type custom_complex_type =
   | ComplexConstructor1 of custom_simple_type
   | ComplexConstructor2 of int * bool * unit * custom_simple_type
   "
   in
   let ast = Parsing.Lex_and_parse.parse_source_code_with_error (Lexing.from_string source_code) in
   match Typing.Typecheck_program.typecheck_program ast with
   | Ok typed_program -> Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program
   | Error error -> Error.raise error *)

open Ast.Ast_types

(* open Core *)
open Typing.Type_context_env

type mock_position = { fname : string; lnum : int; bol : int; cnum : int }

let mock_lexing_position (mp : mock_position) : Lexing.position =
  {
    Lexing.pos_fname = mp.fname;
    pos_lnum = mp.lnum;
    pos_bol = mp.bol;
    pos_cnum = mp.cnum;
  }

(* Example usage *)
let mock_position =
  mock_lexing_position { fname = "mock_file"; lnum = 42; bol = 0; cnum = 100 }
;;

(* Use the mock position in your code or testing *)

let typing_context =
  [
    TypingContextEntry (Var_name.of_string "x2", TEBool mock_position);
    TypingContextEntry
      (Var_name.of_string "x3", TEOption (mock_position, TEInt mock_position));
    TypingContextEntry
      ( Var_name.of_string "x4",
        TECustom (mock_position, Type_name.of_string "custom_type") );
    TypingContextEntry (Var_name.of_string "x5", TEUnit mock_position);
    TypingContextEntry
      ( Var_name.of_string "x6",
        TEArrow (mock_position, TEInt mock_position, TEInt mock_position) );
    TypingContextEntry
      ( Var_name.of_string "x7",
        TEArrow
          ( mock_position,
            TEArrow (mock_position, TEInt mock_position, TEInt mock_position),
            TEInt mock_position ) );
  ]
in
pprint_typing_context Fmt.stdout typing_context

(* match
   let open Result in
   extend_typing_context typing_context (Var_name.of_string "x1") (TEInt(mock_position))
   >>= fun typing_context -> extend_typing_context typing_context (Var_name.of_string "x1") (TEInt(mock_position))
   >>= fun typing_context -> Ok (pprint_typing_context Fmt.stdout typing_context)
   with
   | Error err -> Error.raise err
   | Ok _ -> () *)

(* let open Result in
   get_var_type typing_context (Var_name.of_string "x2")
   >>= fun type_expr -> Ok (print_string (string_of_type type_expr)) *)

(* match
   get_var_type typing_context (Var_name.of_string "x2")
   with
   | Error err -> Error.raise err
   | Ok type_expr -> print_string (string_of_type type_expr) *)
