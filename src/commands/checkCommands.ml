(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open Utils_js

type printer =
  | Json of { pretty: bool; version: Errors.Json_output.json_version option }
  | Cli of Errors.Cli_output.error_flags

(* helper - print errors. used in check-and-die runs *)
let print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings =
  let strip_root =
    if Options.should_strip_root options
    then Some (Options.root options)
    else None
  in

  match printer with
  | Json { pretty; version } ->
    let profiling =
      if options.Options.opt_profile
      then Some profiling
      else None in
    Errors.Json_output.print_errors
      ~out_channel:stdout
      ~strip_root
      ~profiling
      ~pretty
      ?version
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
      ~lazy_msg:None
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
        |> json_version_flag
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> anon "root" (optional string)
      );
    usage = Printf.sprintf
      "Usage: %s check [OPTION]... [ROOT]\n\n\
        Does a full Flow check and prints the results.\n\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n"
        exe_name;
  }

  let main
      error_flags include_suppressed options_flags json pretty json_version
      shm_flags ignore_version from path_opt
      () =

    let root = CommandUtils.guess_root path_opt in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
    let options = make_options ~flowconfig ~lazy_mode:None ~root options_flags in

    if Options.should_profile options && not Sys.win32
    then begin
      Flow_server_profile.init ();
      let rec sample_processor_info () =
        Flow_server_profile.processor_sample ();
        Timer.set_timer ~interval:1.0 ~callback:sample_processor_info |> ignore
      in
      sample_processor_info ();
    end;

    (* initialize loggers before doing too much, especially anything that might exit *)
    LoggingUtils.init_loggers ~from ~options ~min_level:Hh_logger.Level.Error ();

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in

    let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in

    let profiling, errors, warnings, suppressed_errors = Server.check_once
      ~shared_mem_config ~client_include_warnings options in
    let suppressed_errors =
      if include_suppressed then suppressed_errors else [] in
    let printer =
      if json || Option.is_some json_version || pretty then
        Json { pretty; version = json_version }
      else
        Cli error_flags in
    print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings;
    Flow_server_profile.print_url ();
    FlowExitStatus.exit (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)

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
        |> json_version_flag
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> root_flag
        |> input_file_flag "check"
        |> anon "root" (list_of string)
      );
    usage = Printf.sprintf
      "Usage: %s focus-check [OPTION]... [FILES/DIRS]\n\n\
        EXPERIMENTAL: Does a focused Flow check on the input files/directories (and each of their \
        dependents and dependencies) and prints the results.\n\n\
        If --root is not specified, Flow will search upward for a .flowconfig file from the first \
        file or dir in FILES/DIR.\n\
        If --root is not specified and FILES/DIR is omitted, a focus check is ran on the current \
        directory.\n"
        exe_name;
  }

  let main
      error_flags include_suppressed options_flags json pretty json_version
      shm_flags ignore_version from root input_file filenames
      () =

    let filenames = get_filenames_from_input input_file filenames in

    (* If --root is explicitly set, then use that as the root. Otherwise, use the first file *)
    let root = CommandUtils.guess_root (
      if root <> None
      then root
      else match filenames with [] -> None | x::_ -> Some x
    ) in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
    let options = make_options ~flowconfig ~lazy_mode:None ~root options_flags in

    (* initialize loggers before doing too much, especially anything that might exit *)
    LoggingUtils.init_loggers ~from ~options ();

    (* do this after loggers are initialized, so we can complain properly *)
    let file_options = Options.file_options options in
    let filenames = expand_file_list filenames ~options:file_options in
    Hh_logger.info "Checking %d files" (SSet.cardinal filenames);

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in

    let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in

    let focus_targets = SSet.fold
      (fun file acc -> FilenameSet.add (File_key.SourceFile Path.(to_string (make file))) acc)
      filenames
      FilenameSet.empty in

    let profiling, errors, warnings, suppressed_errors = Server.check_once
      ~shared_mem_config ~client_include_warnings ~focus_targets options in
    let suppressed_errors =
      if include_suppressed then suppressed_errors else [] in
    let printer =
      if json || Option.is_some json_version || pretty then
        Json { pretty; version = json_version }
      else
        Cli error_flags
    in
    print_errors ~printer ~profiling ~suppressed_errors options ~errors ~warnings;
    FlowExitStatus.exit (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)
  let command = CommandSpec.command spec main
end
