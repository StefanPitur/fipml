open Core;;

let channel = In_channel.create "src/main.fipml" in
match
  Parsing.Lex_and_parse.parse_source_code_with_error
    (Lexing.from_channel channel)
with
| Ok parsed_program -> (
    match Typing.Typecheck_program.typecheck_program parsed_program with
    | Error err -> print_string (Error.to_string_hum err)
    | Ok typed_program ->
        Typing.Pprint_typed_ast.pprint_typed_program Fmt.stdout typed_program)
| Error err -> print_string (Error.to_string_hum err)
(*
(* open Ast.Ast_types
   open Core
   open Fip.Borrowed_context;;

   let initial_borrowed_context = BorrowedSet.empty in
   let extended_borrowed_context =
     extend_borrowed_set ~element:(Var_name.of_string "x")
       ~borrowed_set:initial_borrowed_context
   in

   print_string (Int.to_string (Set.length initial_borrowed_context));
   List.iter (Set.elements initial_borrowed_context) ~f:(fun var_name ->
       print_string (Var_name.to_string var_name));
   print_string (Int.to_string (Set.length extended_borrowed_context));
   List.iter (Set.elements extended_borrowed_context) ~f:(fun var_name ->
       print_string (Var_name.to_string var_name)) *)

open Core
open Fip.Reuse_credits;;

let initial_reuse_map = ReuseMap.empty in
let extended_reuse_map =
  extend_reuse_map ~reuse_size:5
    ~reuse_map:(extend_reuse_map ~reuse_size:5 ~reuse_map:initial_reuse_map)
in
let final_reuse_map =
  extend_reuse_map ~reuse_size:10 ~reuse_map:extended_reuse_map
in

print_string (Int.to_string (Map.length initial_reuse_map) ^ "\n");
Map.iteri initial_reuse_map ~f:(fun ~key ~data ->
    print_string (Int.to_string key ^ " - " ^ Int.to_string data ^ "\n"));
print_string "\n";
print_string (Int.to_string (Map.length extended_reuse_map) ^ "\n");
Map.iteri extended_reuse_map ~f:(fun ~key ~data ->
    print_string (Int.to_string key ^ " - " ^ Int.to_string data ^ "\n"));
print_string "\n";
print_string (Int.to_string (Map.length final_reuse_map) ^ "\n");
Map.iteri final_reuse_map ~f:(fun ~key ~data ->
    print_string (Int.to_string key ^ " - " ^ Int.to_string data ^ "\n"));
print_string "\n";

let combined =
  combine_reuse_maps ~reuse_map1:extended_reuse_map ~reuse_map2:final_reuse_map
in
print_string (Int.to_string (Map.length combined) ^ "\n");
Map.iteri combined ~f:(fun ~key ~data ->
    print_string (Int.to_string key ^ " - " ^ Int.to_string data ^ "\n"));
print_string "\n"
*)
