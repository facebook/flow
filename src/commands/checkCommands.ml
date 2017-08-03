(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open CommandUtils
open Utils_js

module Main = ServerFunctors.ServerMain (Server.FlowProgram)

type printer =
  | Json of { pretty: bool }
  | Cli of Errors.Cli_output.error_flags

(* helper - print errors. used in check-and-die runs *)
let print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings =
  let strip_root =
    if Options.should_strip_root options
    then Some (Options.root options)
    else None
  in

  match printer with
  | Json { pretty } ->
    let profiling =
      if options.Options.opt_profile
      then Some profiling
      else None in
    Errors.Json_output.print_errors
      ~out_channel:stdout
      ~strip_root
      ~profiling
      ~pretty
      ~suppressed_errors
      ~errors
      ~warnings
      ()
  | Cli flags ->
    let errors = List.fold_left
      (fun acc (error, _) -> Errors.ErrorSet.add error acc)
      errors
      suppressed_errors
    in
    Errors.Cli_output.print_errors
      ~out_channel:stdout
      ~flags
      ~strip_root
      ~errors
      ~warnings
      ()

module CheckCommand = struct
  let spec = { CommandSpec.
    name = "check";
    doc = "Does a full Flow check and prints the results";
    args = CommandSpec.ArgSpec.(
        empty
        |> error_flags
        |> flag "--include-suppressed" no_arg
          ~doc:"Ignore any `suppress_comment` lines in .flowconfig"
        |> options_and_json_flags
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> anon "root" (optional string) ~doc:"Root directory"
      );
    usage = Printf.sprintf
      "Usage: %s check [OPTION]... [ROOT]\n\n\
        Does a full Flow check and prints the results.\n\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n"
        exe_name;
  }

  let main
      error_flags include_suppressed options_flags json pretty
      shm_flags ignore_version from path_opt
      () =

    let root = CommandUtils.guess_root path_opt in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
    let options = make_options ~flowconfig ~lazy_:false ~root options_flags in

    if Options.should_profile options then Flow_server_profile.init ();

    (* initialize loggers before doing too much, especially anything that might exit *)
    init_loggers ~from ~options ~min_level:Hh_logger.Level.Error ();

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in

    let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in

    let profiling, errors, warnings, suppressed_errors = Main.check_once
      ~shared_mem_config ~client_include_warnings options in
    let suppressed_errors =
      if include_suppressed then suppressed_errors else [] in
    let printer =
      if json || pretty then Json { pretty } else Cli error_flags in
    print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings;
    if Errors.ErrorSet.is_empty errors
      then FlowExitStatus.(exit No_error)
      else FlowExitStatus.(exit Type_error)

  let command = CommandSpec.command spec main
end

module FocusCheckCommand = struct
  let spec = { CommandSpec.
    name = "focus-check";
    doc = "EXPERIMENTAL: " ^
      "Does a focused Flow check on a file (and its dependents and their dependencies) " ^
      "and prints the results";
    args = CommandSpec.ArgSpec.(
        empty
        |> error_flags
        |> flag "--include-suppressed" no_arg
          ~doc:"Ignore any `suppress_comment` lines in .flowconfig"
        |> options_and_json_flags
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> flag "--input-file" string
          ~doc:("File containing list of files to transform, one per line. If -, list of files is "^
            "read from the standard input.")
        |> anon "root" (list_of string) ~doc:"Root directory"
      );
    usage = Printf.sprintf
      "Usage: %s focus-check [OPTION]... [FILES/DIRS]\n\n\
        EXPERIMENTAL: Does a focused Flow check on the input files/directories (and each of their \
        dependents and dependencies) and prints the results.\n\n\
        Flow will search upward for a .flowconfig file, at the first file specified.\n\
        If no file is specified, a focus check is ran on the current directory.\n"
        exe_name;
  }

  let main
      error_flags include_suppressed options_flags json pretty
      shm_flags ignore_version from input_file filenames
      () =

    let filenames = get_filenames_from_input input_file filenames in
    let file_opt = match filenames with
    | [] -> None
    | x::_ -> Some x in
    let root = CommandUtils.guess_root file_opt in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
    let options = make_options ~flowconfig ~lazy_:false ~root options_flags in

    (* initialize loggers before doing too much, especially anything that might exit *)
    init_loggers ~from ~options ();

    (* do this after loggers are initialized, so we can complain properly *)
    let file_options = Options.file_options options in
    let filenames = expand_file_list filenames ~options:file_options in
    Hh_logger.info "Checking %d files" (SSet.cardinal filenames);

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in

    let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in

    let filenames = SSet.elements filenames in
    let focus_targets = List.map (fun file ->
      (Loc.SourceFile Path.(to_string (make file)))) filenames
    in

    let profiling, errors, warnings, suppressed_errors = Main.check_once
      ~shared_mem_config ~client_include_warnings ~focus_targets options in
    let suppressed_errors =
      if include_suppressed then suppressed_errors else [] in
    let printer =
      if json || pretty then Json { pretty } else Cli error_flags in
    print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings;
    if Errors.ErrorSet.is_empty errors
      then FlowExitStatus.(exit No_error)
      else FlowExitStatus.(exit Type_error)

  let command = CommandSpec.command spec main
end
