open Ast.Ast_types
open Core

exception ReuseCreditsNotAvailable of string
exception ReuseMapsNotEqual

module ReuseMap = Map.Make (struct
  type t = int

  let compare = Int.compare
  let sexp_of_t x = Int.sexp_of_t x
  let t_of_sexp x = Int.t_of_sexp x
end)

type reuse_map_entry = int * Var_name.t list

let allocation_credit_size () = -1

let extend_reuse_map ~(reuse_size : int) ~(reuse_var : Var_name.t)
    ~(reuse_map : 'a ReuseMap.t) : 'a ReuseMap.t =
  Map.update reuse_map reuse_size ~f:(fun reuse_size_count_option ->
      match reuse_size_count_option with
      | None -> (1, [ reuse_var ])
      | Some (reuse_size_count, reuse_vars) ->
          (reuse_size_count + 1, reuse_var :: reuse_vars))

let rec extend_reuse_map_k_times ~(reuse_size : int) ~(reuse_var : Var_name.t)
    ~(k : int) ~(reuse_map : 'a ReuseMap.t) : 'a ReuseMap.t =
  match k with
  | 0 -> reuse_map
  | _ ->
      extend_reuse_map ~reuse_size ~reuse_var
        ~reuse_map:
          (extend_reuse_map_k_times ~reuse_size ~reuse_var ~k:(k - 1) ~reuse_map)

let consume_reuse_map ~(reuse_size : int)
    ~(reuse_map : reuse_map_entry ReuseMap.t) :
    (Var_name.t * reuse_map_entry ReuseMap.t) Or_error.t =
  let reuse_var_ref = ref (Var_name.of_string "") in
  if reuse_size = 0 then Ok (!reuse_var_ref, reuse_map)
  else
    let updated_map =
      Map.update reuse_map reuse_size ~f:(fun reuse_size_count_option ->
          Or_error.ok_exn
            (match reuse_size_count_option with
            | Some (reuse_size_count, reuse_var :: reuse_vars)
              when reuse_size_count >= 1 ->
                reuse_var_ref := reuse_var;
                Ok (reuse_size_count - 1, reuse_vars)
            | _ ->
                Or_error.of_exn
                  (ReuseCreditsNotAvailable (Int.to_string reuse_size))))
    in
    match Map.find updated_map reuse_size with
    | Some (0, []) -> Ok (!reuse_var_ref, Map.remove updated_map reuse_size)
    | _ -> Ok (!reuse_var_ref, updated_map)

let combine_reuse_maps ~(reuse_map1 : reuse_map_entry ReuseMap.t)
    ~(reuse_map2 : reuse_map_entry ReuseMap.t) : reuse_map_entry ReuseMap.t =
  (* Append order may differ *)
  Map.merge_skewed reuse_map1 reuse_map2
    ~combine:(fun ~key:_ (v1, reuse_vars1) (v2, reuse_vars2) ->
      (v1 + v2, reuse_vars1 @ reuse_vars2))

let reuse_map_entry_equal_fn (reuse_map_entry1 : reuse_map_entry)
    (reuse_map_entry2 : reuse_map_entry) : bool =
  let c1, vs1 = reuse_map_entry1 in
  let c2, vs2 = reuse_map_entry2 in
  Int.( = ) c1 c2 && List.equal Var_name.( = ) vs1 vs2

let reuse_map_equal_fn (reuse_map1 : reuse_map_entry ReuseMap.t)
    (reuse_map2 : reuse_map_entry ReuseMap.t) : bool =
  Map.equal reuse_map_entry_equal_fn reuse_map1 reuse_map2

let assert_reuse_maps_are_equal ~(reuse_map1 : reuse_map_entry ReuseMap.t)
    ~(reuse_map2 : reuse_map_entry ReuseMap.t) : unit Or_error.t =
  match reuse_map_equal_fn reuse_map1 reuse_map2 with
  | true -> Ok ()
  | false -> Or_error.of_exn ReuseMapsNotEqual

let string_of_reuse_map_entry (reuse_map_entry : reuse_map_entry) : string =
  let reuse_count, reuse_vars = reuse_map_entry in
  Fmt.str "Count = %i - [%s]" reuse_count
    (String.concat ~sep:", "
       (List.map reuse_vars ~f:Ast.Ast_types.Var_name.to_string))

let string_of_reuse_map (reuse_map : reuse_map_entry ReuseMap.t)
    (indent : string) : string =
  let key_data_list = Map.to_alist reuse_map in
  let reuse_map_entrys =
    List.map key_data_list ~f:(fun (key, data) ->
        Fmt.str "%sKey = %s :: %s" indent (Int.to_string key)
          (string_of_reuse_map_entry data))
  in
  String.concat ~sep:"\n" reuse_map_entrys

let pprint_reuse_map (ppf : Format.formatter) ~(indent : string)
    (reuse_map : reuse_map_entry ReuseMap.t) : unit =
  let reuse_map_string = string_of_reuse_map reuse_map indent in
  Fmt.pf ppf "%sReuse\n%s@." indent reuse_map_string
