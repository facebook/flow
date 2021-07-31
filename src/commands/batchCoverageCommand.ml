(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "batch-coverage";
    doc = "Shows aggregate coverage information for a group of files or directories ";
    usage =
      Printf.sprintf
        "Usage: %s batch-coverage [OPTION]... [FILE...] \n\ne.g. %s batch-coverage foo.js bar.js baz.js dirname1 dirname2 --show-all \nor   %s batch-coverage --input-file filenames.txt\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags_no_lazy
        |> json_flags
        |> root_flag
        |> strip_root_flag
        |> wait_for_recheck_flag
        |> flag
             "--input-file"
             string
             ~doc:
               "File containing list of files or directories to compute coverage for, one per line. If -, the list is read from standard input."
        |> flag
             "--show-all"
             no_arg
             ~doc:
               "Whether to output the coverage for all files. If not specified, this command will only print coverage for 50 files. "
        |> flag
             "--show-trust"
             no_arg
             ~doc:"EXPERIMENTAL: Whether to include trust information in output"
        |> anon "FILE..." (list_of string));
  }

let output_results ~root ~strip_root ~json ~pretty ~show_all ~trust stats =
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  let percent top bottom =
    if bottom = 0 then
      0.
    else
      float_of_int top /. float_of_int bottom *. 100.
  in
  (* Compute aggregate stats *)
  let (trusted, untrusted, any, empty) =
    Base.List.fold_left
      ~f:
        (fun (acc_trust, acc_untrust, acc_any, acc_empty)
             (_, { Coverage_response.untainted; tainted; empty; uncovered }) ->
        (acc_trust + untainted, acc_untrust + tainted, acc_any + uncovered, acc_empty + empty))
      stats
      ~init:(0, 0, 0, 0)
  in
  let num_files_in_dir = Base.List.length stats in
  let covered = trusted + untrusted in
  let total = covered + any + empty in
  let trusted_percentage = percent trusted total in
  let percentage = percent covered total in
  let file_stats (file_key, { Coverage_response.untainted; tainted; empty; uncovered }) =
    let covered = untainted + tainted in
    let total = covered + uncovered + empty in
    let percentage = percent covered total in
    let trusted_percentage =
      if trust then
        Some (percent untainted total)
      else
        None
    in
    let file = Reason.string_of_source ~strip_root file_key in
    let untainted =
      if trust then
        Some untainted
      else
        None
    in
    (file, untainted, covered, total, trusted_percentage, percentage)
  in
  if json then
    Hh_json.(
      let file_to_json stats =
        let (file, trusted, covered, total, trusted_percentage, percentage) = file_stats stats in
        let percentage = [("percentage", JSON_Number (spf "%0.2f" percentage))] in
        let percentage =
          match trusted_percentage with
          | Some p -> ("trusted_percentage", JSON_Number (spf "%0.2f" p)) :: percentage
          | None -> percentage
        in
        let covered = [("covered", int_ covered)] in
        let covered =
          match trusted with
          | Some t -> ("trusted", int_ t) :: covered
          | None -> covered
        in
        JSON_Object ([("file", string_ file)] @ percentage @ covered @ [("total", int_ total)])
      in
      let array_ elts = JSON_Array elts in
      let file_list =
        Base.List.sort ~compare:(fun (a, _) (b, _) -> Stdlib.compare a b) stats
        |> Base.List.map ~f:file_to_json
      in
      let covered_expressions = [("covered_expressions", int_ covered)] in
      let covered_expressions =
        if trust then
          ("trusted_expressions", int_ trusted) :: covered_expressions
        else
          covered_expressions
      in
      let percentage = [("percentage", JSON_Number (spf "%0.2f" percentage))] in
      let percentage =
        if trust then
          ("trusted_percentage", JSON_Number (spf "%0.2f" trusted_percentage)) :: percentage
        else
          percentage
      in
      let json_output =
        JSON_Object
          [
            ("files", array_ file_list);
            ( "statistics",
              JSON_Object
                ([("files_in_directory", int_ num_files_in_dir)]
                @ covered_expressions
                @ [("total_expressions", int_ total)]
                @ percentage) );
          ]
      in
      print_json_endline ~pretty json_output)
  else
    let (truncation_text, truncated_stats) =
      if num_files_in_dir > 50 && not show_all then
        ( spf
            "\nOnly showing coverage for 50 of %d files. To show more, rerun with --show-all.\n"
            num_files_in_dir,
          Base.List.take stats 50 )
      else
        ("", stats)
    in
    if num_files_in_dir > 0 then (
      print_endlinef "\nCoverage results from %d file(s):\n" num_files_in_dir;
      Base.List.iter
        ~f:(fun fstats ->
          let (file, trusted, covered, total, trusted_percentage, percentage) = file_stats fstats in
          match (trusted_percentage, trusted) with
          | (Some p, Some t) ->
            print_endlinef
              "%s: %0.2f%% trusted (%d of %d expressions), %0.2f%% covered (%d of %d expressions)"
              file
              p
              t
              total
              percentage
              covered
              total
          | _ -> print_endlinef "%s: %0.2f%% (%d of %d expressions)" file percentage covered total)
        truncated_stats;
      print_endline truncation_text
    );

    print_endlinef "-----------------------------------";
    print_endlinef "Aggregate coverage statistics";
    print_endlinef "-----------------------------------";
    print_endlinef "Files                : %d" num_files_in_dir;
    print_endlinef "Expressions          :";
    if trust then print_endlinef "  Trusted            : %d" trusted;
    print_endlinef "  Covered            : %d" covered;
    print_endlinef "  Total              : %d" total;
    if trust then print_endlinef "  Trust Percentage   : %0.2f%%" trusted_percentage;
    print_endlinef "  Covered Percentage : %0.2f%%" percentage;
    print_endlinef ""

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    wait_for_recheck
    input
    show_all
    trust
    files
    () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let batch =
    get_filenames_from_input input files |> Base.List.map ~f:(Path.make %> Path.to_string)
  in
  let input = Base.Option.map (Base.List.hd batch) (fun x -> File_input.FileName x) in
  let root = get_the_root ~base_flags ?input root in
  (* pretty implies json *)
  let json = json || pretty in
  let request = ServerProt.Request.BATCH_COVERAGE { batch; wait_for_recheck; trust } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.BATCH_COVERAGE (Error msg) -> Exit.(exit ~msg Unknown_error)
  | ServerProt.Response.BATCH_COVERAGE (Ok resp) ->
    output_results ~root ~strip_root ~json ~pretty ~show_all ~trust resp
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
