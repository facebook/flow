(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "batch-coverage";
  doc = "Shows aggregate coverage information for a group of files or directories ";
  usage = Printf.sprintf
    "Usage: %s batch-coverage [OPTION]... [FILE...] \n\n\
      e.g. %s batch-coverage foo.js bar.js baz.js dirname1 dirname2 --show-all \n\
      or   %s batch-coverage --input-file filenames.txt\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> base_flags
    |> connect_flags_no_lazy
    |> json_flags
    |> root_flag
    |> strip_root_flag
    |> wait_for_recheck_flag
    |> flag "--input-file" string
       ~doc:("File containing list of files or directories to compute coverage for, one per line. \
        If -, the list is read from standard input.")
    |> flag "--show-all" no_arg ~doc:("Whether to output the coverage for all files. If not \
        specified, this command will only print coverage for 50 files. ")
    |> anon "FILE..." (list_of string)
  )
}

let output_results ~root ~strip_root ~json ~pretty ~show_all stats =
  let strip_root = if strip_root then Some root else None in
  let percent top bottom =
    if bottom = 0 then 0. else (float_of_int top /. float_of_int bottom) *. 100. in
  (* Compute aggregate stats *)
  let covered, any, empty = Core_list.fold_left
    ~f:(fun (acc_cov, acc_any, acc_empty) (_, { Coverage.covered; empty; any }) ->
      (acc_cov + covered, acc_any + any, acc_empty + empty)
  ) stats ~init:(0, 0, 0) in
  let num_files_in_dir = Core_list.length stats in
  let total = covered + any + empty in
  let percentage = percent covered total in

  let file_stats (file_key, { Coverage.covered; empty; any }) =
    let total = covered + any + empty in
    let percent = percent covered total in
    let file = Reason.string_of_source ~strip_root file_key in
    file, covered, total, percent in

  if json then begin
    let open Hh_json in
    let file_to_json stats =
      let file, covered, total, percentage = file_stats stats in
      JSON_Object [
        "file", string_ file;
        "percentage", JSON_Number (spf "%0.2f" percentage);
        "covered", int_ covered;
        "total", int_ total;
      ] in
    let array_ elts = JSON_Array elts in
    let file_list =
      Core_list.sort ~cmp:(fun (a, _) (b, _) -> Pervasives.compare a b) stats
      |> Core_list.map ~f:file_to_json in
    let json_output = JSON_Object [
      "files", array_ file_list;
      "statistics", JSON_Object [
        "files_in_directory", int_ num_files_in_dir;
        "covered_expressions", int_ covered;
        "total_expressions", int_ total;
        "percentage", JSON_Number (spf "%0.2f" percentage);
      ]
    ] in
    print_json_endline ~pretty json_output
  end
  else begin
    let truncation_text, truncated_stats =
      if num_files_in_dir > 50 && not show_all then
        spf "\nOnly showing coverage for 50 of %d files. To show more, rerun with --show-all.\n"
          num_files_in_dir,
        Core_list.take stats 50
      else "", stats in
    if num_files_in_dir > 0 then begin
      print_endlinef "\nCoverage results from %d file(s):\n" num_files_in_dir;
      Core_list.iter ~f:(fun fstats ->
        let file, covered, total, percent = file_stats fstats in
        print_endlinef "%s: %0.2f%% (%d of %d expressions)" file percent covered total;
      ) truncated_stats;
      print_endline truncation_text;
    end;

    print_endlinef "-----------------------------------";
    print_endlinef "Aggregate coverage statistics";
    print_endlinef "-----------------------------------";
    print_endlinef "Files        : %d" num_files_in_dir;
    print_endlinef "Expressions  :";
    print_endlinef "  Covered    : %d" covered;
    print_endlinef "  Total      : %d" total;
    print_endlinef "  Percentage : %0.2f%%" percentage;
    print_endlinef ""
  end
let main base_flags option_values json pretty root strip_root
  wait_for_recheck input show_all files () =

  let flowconfig_name = base_flags.Base_flags.flowconfig_name in

  let batch =
    get_filenames_from_input input files
    |> Core_list.map ~f:(Path.make %> Path.to_string) in

  let root = match root with
  | None -> Core_list.hd batch |> CommandUtils.guess_root flowconfig_name
  | Some provided_root ->
    let dir = Path.make provided_root in
    if Path.file_exists (Path.concat dir flowconfig_name) then dir
    else begin
      let msg = spf "Invalid root directory %s" provided_root in
      FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
    end in

  (* pretty implies json *)
  let json = json || pretty in

  let request = ServerProt.Request.BATCH_COVERAGE { batch; wait_for_recheck; } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.BATCH_COVERAGE (Error msg) ->
      FlowExitStatus.(exit ~msg Unknown_error)
  | ServerProt.Response.BATCH_COVERAGE (Ok resp) ->
      output_results ~root ~strip_root ~json ~pretty ~show_all resp
  | response ->
      failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
