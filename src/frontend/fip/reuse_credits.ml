open Ast.Ast_types
open Core

exception ReuseCreditsNotAvailable of string

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
    reuse_map_entry ReuseMap.t Or_error.t =
  Ok
    (Map.update reuse_map reuse_size ~f:(fun reuse_size_count_option ->
         Or_error.ok_exn
           (match reuse_size_count_option with
           | Some (reuse_size_count, _ :: reuse_vars) when reuse_size_count >= 1
             ->
               Ok (reuse_size_count - 1, reuse_vars)
           | _ ->
               Or_error.of_exn
                 (ReuseCreditsNotAvailable (Int.to_string reuse_size)))))

let combine_reuse_maps ~(reuse_map1 : reuse_map_entry ReuseMap.t)
    ~(reuse_map2 : reuse_map_entry ReuseMap.t) : reuse_map_entry ReuseMap.t =
  (* Append order may differ *)
  Map.merge_skewed reuse_map1 reuse_map2
    ~combine:(fun ~key:_ (v1, reuse_vars1) (v2, reuse_vars2) ->
      (v1 + v2, reuse_vars1 @ reuse_vars2))
