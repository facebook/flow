(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SyntaxTree = Full_fidelity_syntax_tree
module SourceText = Full_fidelity_source_text

module AstGenGrammar = Hack_grammar_descriptor_for_old
module AstGen = Random_ast_generator.Make(AstGenGrammar)
module Config = Random_ast_generator_config

module IntCompare = struct
  type t = int
  let compare = Pervasives.compare
end
module IntStore = Map.Make(IntCompare)

module TupleCompare = struct
  type t = int * int
  let compare = Pervasives.compare
end
module TupleStore = Map.Make(TupleCompare)


let usage =
  "Usage: temp_file target_folder max_file loop seed
   temp_file: Temporary file location for hh_single_parse to use.
   target_folder: Path to folder that store generated test case.
   max_file: Maximum number of files for given (seed, size).
   loop: Flag to indicate whether to loop through all seeds during generation.
   low: desired shortest size of program (not guaranteed to be actual size).
   high: desired largest size of program (not guaranteed to be actual size).
   interval: step by which size of program increases.
   repeat: number of generations run for each step.
   seed: The seed to use if no looping, the number of seed if looping."

let round granularity x = x / granularity * granularity

let generate_source_with_count count config =
  AstGen.generate_with_exact_count count config

let stringfy_error errorl =
  let fold_fun acc error =
    let new_log = error |> Errors.to_absolute |> Errors.to_string in
    acc ^ new_log
  in
  List.fold_left fold_fun "" (Errors.get_error_list errorl)

let old_parse_with_source source path =
  (* the old parser seems to parse only from files, thus this trick *)
  let channel = open_out path in
  let _ = Pervasives.output_string channel source in
  let _ = close_out channel in
  let file = Relative_path.create Relative_path.Dummy path in
  let errorl, result, _ =
    Errors.do_ begin fun () ->
      (* FIXME: Don't use default tcopt *)
      Parser_hack.from_file TypecheckerOptions.default file
    end
  in
  (Errors.is_empty errorl, stringfy_error errorl)

let count_existing_files_in_folder seed size max_file folder =
  let all_files = Sys.readdir folder in
  let all_files = Array.to_list all_files in
  let is_current_seed_size filename =
    let filename_prefix = Printf.sprintf "test_%d_%d_" size seed in
    String_utils.string_starts_with filename filename_prefix
  in
  let satisfied_list = List.filter is_current_seed_size all_files in
  let list_count = List.length satisfied_list in
  if list_count > max_file then None
  else Some list_count

(* file name format: test_<size>_<seed>_<index>.php *)
let count_existing_files seed size max_file mem folder =
  (* do not read the file system if it has been read before *)
  let index_opt =
    if TupleStore.mem (seed, size) mem then
      Some (TupleStore.find (seed, size) mem)
    else
      count_existing_files_in_folder seed size max_file folder
  in
  match index_opt with
  | None -> mem, None
  | Some index when index >= max_file -> mem, None
  | Some index ->
    let next_index = index + 1 in
    (TupleStore.add (seed, size) next_index mem, Some index)


let save_to_file seed size target_folder source max_file mem no_error =
  let mem, index_opt =
    if no_error then count_existing_files seed size max_file mem target_folder
    else mem, Some 0
  in
  match index_opt with
  | None -> mem
  | Some index ->
    let target_file =
      if no_error then
        Printf.sprintf "%s/test_%d_%d_%d.txt" target_folder size seed index
      else
        Printf.sprintf "%s/error_log.txt" target_folder
    in
    let open_flag =
      if no_error then [Open_creat; Open_wronly] else [Open_creat; Open_append]
    in
    let channel = open_out_gen open_flag 0o666 target_file in
    output_string channel source;
    close_out channel;
    mem

let put_in_store result_store real_count time_new time_old =
  if IntStore.mem real_count result_store then
    let (num, total_time_new, total_time_old) =
      IntStore.find real_count result_store in
    let new_aggregate =
      (num + 1, total_time_new +. time_new, total_time_old +. time_old) in
    IntStore.add real_count new_aggregate result_store
  else
    IntStore.add real_count (1, time_new, time_old) result_store

let rec n_times f x acc n =
  if n = 0 then acc
  else
    let acc = f x acc in
    n_times f x acc (n - 1)

let run low high interval repeat path target_folder seed max_file config =
  let round = round interval in
  let rec aux current mem =
    if current > high then mem
    else
      let (source, real_count) = generate_source_with_count current config in
      let real_count = round real_count in
      let (no_error, errorl) = old_parse_with_source source path in
      let source =
        if no_error then Printf.sprintf "%s\n" source
        else Printf.sprintf "%s\n%s\n" source errorl
      in
      let mem =
        save_to_file seed real_count target_folder source max_file mem no_error
      in
      aux (current + interval) mem

  in
  n_times aux low TupleStore.empty repeat

let run_test low high interval repeat path target_folder seed max_file config =
  let store =
    run low high interval repeat path target_folder seed max_file config in
  let iter_fun (seed, size) count =
    Printf.printf "(size, seed): (%d, %d); total_count: %d\n" size seed count in
  TupleStore.iter iter_fun store


let gen_seed seed =
  Random.init seed;
  seed

let run_main
  low high interval repeat filename target_folder seed max_file config =
  let seed = gen_seed seed in
  run_test low high interval repeat filename target_folder seed max_file config

let loop_main
  (* use [seed_range] many seeds *)
  low high interval repeat filename target_folder seed_range max_file config =
  for num = 1 to seed_range do
    let seed = (Random.int 10000000) + 1 (* some large number *) in
    run_main low high interval repeat filename target_folder seed max_file
      config
  done

let main
  low high interval repeat filename target_folder seed max_file do_loop config =
  let config =
    try Config.find_config config
    with _ ->
      let error_msg = Printf.sprintf "Config '%s' not found." config in
      let reminder = "Make sure your case-sensitive spelling is correct." in
      failwith (Printf.sprintf "%s %s" error_msg reminder)
  in
  EventLogger.init (Daemon.devnull ()) 0.0;
  let _ = SharedMem.(init GlobalConfig.default_sharedmem_config) in
  if (String.length filename) = 0 ||
      String.length target_folder = 0 ||
      not (Sys.is_directory target_folder) then begin
    Printf.eprintf "%s" usage;
    exit 1
  end;
  (* Note that seed are interpreted differently here.
   * TODO a better way to specify two different options *)
  let _ =
    if do_loop then
      Unix.handle_unix_error
        loop_main low high interval repeat filename target_folder seed max_file
        config
    else
      Unix.handle_unix_error
        run_main low high interval repeat filename target_folder seed max_file
        config
  in
  Sys.remove filename

let () =
  let low, high, interval, repeat,
      filename, target_folder, max_file, do_loop, seed, config =
    try
      (* TODO there has to be a better way *)
      let filename = Sys.argv.(1) in
      let target_folder = Sys.argv.(2) in
      let max_file = int_of_string Sys.argv.(3) in
      let do_loop = bool_of_string Sys.argv.(4) in
      let low = int_of_string Sys.argv.(5) in
      let high = int_of_string Sys.argv.(6) in
      let interval = int_of_string Sys.argv.(7) in
      let repeat = int_of_string Sys.argv.(8) in
      let given_seed = int_of_string Sys.argv.(9) in
      let config = Sys.argv.(10) in
      low, high, interval, repeat, filename, target_folder, max_file, do_loop,
      given_seed, config
    with
    | _ -> Printf.printf "%s\n" usage; exit 1
  in
  main low high interval repeat filename target_folder seed max_file do_loop
    config
