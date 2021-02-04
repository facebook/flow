(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open Utils_js

type printer =
  | Json of {
      pretty: bool;
      version: Errors.Json_output.json_version option;
    }
  | Cli of Errors.Cli_output.error_flags

(* helper - print errors. used in check-and-die runs *)
let format_errors
    ~printer ~client_include_warnings ~offset_kind options (errors, warnings, suppressed_errors) =
  let include_warnings = client_include_warnings || Options.should_include_warnings options in
  let warnings =
    if include_warnings then
      warnings
    else
      Errors.ConcreteLocPrintableErrorSet.empty
  in
  let suppressed_errors =
    if Options.include_suppressions options then
      suppressed_errors
    else
      []
  in
  let strip_root =
    if Options.should_strip_root options then
      Some (Options.root options)
    else
      None
  in
  (* The print_errors functions in the Errors modules are carefully defined to
   * perform any expensive, non-printing work when partially applied, before
   * receiving the `profiling` argument.
   *
   * We use this trick in order to actually profile this work. So, the
   * annotation on the `print_errors` binding below serves to ensure that the
   * error functions are applied enough that this expensive work happens. *)
  let print_errors : Profiling_js.finished option -> unit =
    match printer with
    | Json { pretty; version } ->
      let finish_formatting =
        Errors.Json_output.format_errors
          ~out_channel:stdout
          ~strip_root
          ~pretty
          ?version
          ~offset_kind
          ~suppressed_errors
          ~errors
          ~warnings
          ()
      in
      fun profiling ->
        let profiling_props =
          Base.Option.value_map profiling ~default:[] ~f:Profiling_js.to_legacy_json_properties
        in
        finish_formatting ~profiling_props
    | Cli flags ->
      let errors =
        List.fold_left
          (fun acc (error, _) -> Errors.ConcreteLocPrintableErrorSet.add error acc)
          errors
          suppressed_errors
      in
      let () =
        Errors.Cli_output.format_errors
          ~out_channel:stdout
          ~flags
          ~strip_root
          ~errors
          ~warnings
          ~lazy_msg:None
          ()
      in
      (fun _profiling -> ())
  in
  fun profiling ->
    if options.Options.opt_profile then
      print_errors (Some profiling)
    else
      print_errors None

module CheckCommand = struct
  let spec =
    {
      CommandSpec.name = "check";
      doc = "Does a full Flow check and prints the results";
      args =
        CommandSpec.ArgSpec.(
          empty
          |> base_flags
          |> error_flags
          |> options_and_json_flags
          |> json_version_flag
          |> offset_style_flag
          |> shm_flags
          |> ignore_version_flag
          |> from_flag
          |> no_cgroup_flag
          |> anon "root" (optional string));
      usage =
        Printf.sprintf
          "Usage: %s check [OPTION]... [ROOT]\n\nDoes a full Flow check and prints the results.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n"
          exe_name;
    }

  let main
      base_flags
      error_flags
      options_flags
      json
      pretty
      json_version
      offset_style
      shm_flags
      ignore_version
      path_opt
      () =
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    let root = CommandUtils.guess_root flowconfig_name path_opt in
    let (flowconfig, flowconfig_hash) =
      let flowconfig_path = Server_files_js.config_file flowconfig_name root in
      read_config_and_hash_or_exit ~enforce_warnings:(not ignore_version) flowconfig_path
    in
    let options =
      let lazy_mode = Some Options.NON_LAZY_MODE in
      (* Saved state doesn't make sense for `flow check`, so disable it. *)
      let saved_state_options_flags =
        CommandUtils.Saved_state_flags.
          {
            (* None would mean we would just use the value in the .flowconfig, if available.
             * Instead, let's override that and turn off saved state entirely. *)
            saved_state_fetcher = Some Options.Dummy_fetcher;
            saved_state_force_recheck = false;
            saved_state_no_fallback = false;
          }
      in
      make_options
        ~flowconfig_name
        ~flowconfig_hash
        ~flowconfig
        ~lazy_mode
        ~root
        ~options_flags
        ~saved_state_options_flags
    in
    let init_id = Random_id.short_string () in
    let offset_kind = CommandUtils.offset_kind_of_offset_style offset_style in
    (* initialize loggers before doing too much, especially anything that might exit *)
    LoggingUtils.init_loggers ~options ~min_level:Hh_logger.Level.Error ();

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in
    let format_errors =
      let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in
      let printer =
        if json || Base.Option.is_some json_version || pretty then
          Json { pretty; version = json_version }
        else
          Cli error_flags
      in
      format_errors ~printer ~client_include_warnings ~offset_kind options
    in
    let (errors, warnings) = Server.check_once options ~init_id ~shared_mem_config ~format_errors in
    Flow_server_profile.print_url ();
    FlowExitStatus.exit
      (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)

  let command = CommandSpec.command spec main
end

module FocusCheckCommand = struct
  let spec =
    {
      CommandSpec.name = "focus-check";
      doc =
        "EXPERIMENTAL: "
        ^ "Does a focused Flow check on a file (and its dependents and their dependencies) "
        ^ "and prints the results";
      args =
        CommandSpec.ArgSpec.(
          empty
          |> base_flags
          |> error_flags
          |> options_and_json_flags
          |> json_version_flag
          |> saved_state_flags
          |> offset_style_flag
          |> shm_flags
          |> ignore_version_flag
          |> from_flag
          |> root_flag
          |> input_file_flag "check"
          |> no_cgroup_flag
          |> anon "root" (list_of string));
      usage =
        Printf.sprintf
          "Usage: %s focus-check [OPTION]... [FILES/DIRS]\n\nEXPERIMENTAL: Does a focused Flow check on the input files/directories (and each of their dependents and dependencies) and prints the results.\n\nIf --root is not specified, Flow will search upward for a .flowconfig file from the first file or dir in FILES/DIR.\nIf --root is not specified and FILES/DIR is omitted, a focus check is ran on the current directory.\n"
          exe_name;
    }

  let main
      base_flags
      error_flags
      options_flags
      json
      pretty
      json_version
      saved_state_options_flags
      offset_style
      shm_flags
      ignore_version
      root
      input_file
      filenames
      () =
    let filenames = get_filenames_from_input input_file filenames in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    (* If --root is explicitly set, then use that as the root. Otherwise, use the first file *)
    let root =
      CommandUtils.guess_root
        flowconfig_name
        ( if root <> None then
          root
        else
          match filenames with
          | [] -> None
          | x :: _ -> Some x )
    in
    let (flowconfig, flowconfig_hash) =
      read_config_and_hash_or_exit (Server_files_js.config_file flowconfig_name root)
    in
    let options =
      let lazy_mode = Some Options.NON_LAZY_MODE in
      make_options
        ~flowconfig_name
        ~flowconfig_hash
        ~flowconfig
        ~lazy_mode
        ~root
        ~options_flags
        ~saved_state_options_flags
    in
    let init_id = Random_id.short_string () in
    let offset_kind = CommandUtils.offset_kind_of_offset_style offset_style in
    (* initialize loggers before doing too much, especially anything that might exit *)
    LoggingUtils.init_loggers ~options ();

    (* do this after loggers are initialized, so we can complain properly *)
    let file_options = Options.file_options options in
    let filenames = expand_file_list filenames ~options:file_options in
    Hh_logger.info "Checking %d files" (SSet.cardinal filenames);

    if not ignore_version then assert_version flowconfig;

    let shared_mem_config = shm_config shm_flags flowconfig in
    let focus_targets =
      SSet.fold
        (fun file acc -> FilenameSet.add (File_key.SourceFile Path.(to_string (make file))) acc)
        filenames
        FilenameSet.empty
    in
    let format_errors =
      let client_include_warnings = error_flags.Errors.Cli_output.include_warnings in
      let printer =
        if json || Base.Option.is_some json_version || pretty then
          Json { pretty; version = json_version }
        else
          Cli error_flags
      in
      format_errors ~printer ~client_include_warnings ~offset_kind options
    in
    let (errors, warnings) =
      Server.check_once options ~init_id ~shared_mem_config ~focus_targets ~format_errors
    in
    FlowExitStatus.exit
      (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)

  let command = CommandSpec.command spec main
end
