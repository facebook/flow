(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* server commands *)
(***********************************************************************)

open CommandUtils

type mode = Check | Server | Start | FocusCheck

type printer =
  | Json of { pretty: bool }
  | Cli of Errors.Cli_output.error_flags

module type CONFIG = sig
  val mode : mode
  val default_log_filter : Hh_logger.Level.t -> bool
end

module Main = ServerFunctors.ServerMain (Server.FlowProgram)

(* helper - print errors. used in check-and-die runs *)
let print_errors ~printer ~profiling ~suppressed_errors options errors =
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
      errors
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
      errors

module OptionParser(Config : CONFIG) = struct
  let cmdname = match Config.mode with
  | Check -> "check"
  | Server -> "server"
  | Start -> "start"
  | FocusCheck -> "focus-check"

  let cmddoc = match Config.mode with
  | Check -> "Does a full Flow check and prints the results"
  | Server -> "Runs a Flow server in the foreground"
  | Start -> "Starts a Flow server"
  | FocusCheck -> "EXPERIMENTAL: " ^
    "Does a focused Flow check on a file (and its dependents and their dependencies) " ^
    "and prints the results"

  let common_args prev = CommandSpec.ArgSpec.(
    prev
    |> flag "--debug" no_arg
        ~doc:"Print debug info during typecheck"
    |> flag "--profile" no_arg
        ~doc:"Output profiling information"
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--traces" (optional int)
        ~doc:"Outline an error path up to a specified level"
    |> flag "--lib" (optional string)
        ~doc:"Specify one or more library paths, comma separated"
    |> flag "--no-flowlib" no_arg
        ~doc:"Do not include embedded declarations"
    |> flag "--munge-underscore-members" no_arg
        ~doc:"Treat any class member name with a leading underscore as private"
    |> flag "--max-workers" (optional int)
        ~doc:"Maximum number of workers to create (capped by number of cores)"
    |> ignore_version_flag
    |> flowconfig_flags
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> shm_flags
    |> from_flag
    |> quiet_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )

  let args = match Config.mode with
  | Check -> CommandSpec.ArgSpec.(
      empty
      |> error_flags
      |> flag "--include-suppressed" no_arg
        ~doc:"Ignore any `suppress_comment` lines in .flowconfig"
      |> json_flags
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> dummy false (* lazy *)
      |> common_args
    )
  | Server -> CommandSpec.ArgSpec.(
      empty
      |> dummy Errors.Cli_output.default_error_flags (* error_flags *)
      |> dummy false (* include_suppressed *)
      |> dummy false (* json *)
      |> dummy false (* pretty *)
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> flag "--lazy" no_arg
          ~doc:"EXPERIMENTAL: Don't run a full check"
      |> common_args
    )
  | Start -> CommandSpec.ArgSpec.(
      empty
      |> dummy Errors.Cli_output.default_error_flags (* error_flags *)
      |> dummy false (* include_suppressed *)
      |> json_flags
      |> flag "--log-file" string
          ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.log)"
      |> flag "--wait" no_arg
          ~doc:"Wait for the server to finish initializing"
      |> flag "--lazy" no_arg
          ~doc:"EXPERIMENTAL: Don't run a full check"
      |> common_args
    )
  | FocusCheck -> CommandSpec.ArgSpec.(
      empty
      |> error_flags
      |> flag "--include-suppressed" no_arg
        ~doc:"Ignore any `suppress_comment` lines in .flowconfig"
      |> json_flags
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> dummy false (* lazy *)
      |> common_args
    )

  let spec = {
    CommandSpec.
    name = cmdname;
    doc = cmddoc;
    args;
    usage = Printf.sprintf
      "Usage: %s %s [OPTION]... [ROOT]\n\
        %s\n\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n\
        A server will be started if none is running over ROOT.\n"
        exe_name cmdname cmddoc;
  }

  let assert_version version_constraint =
    if not (Semver.satisfies version_constraint Flow_version.version)
    then
      let msg = Utils_js.spf
        "Wrong version of Flow. The config specifies version %s but this is version %s"
        version_constraint
        Flow_version.version
      in
      FlowExitStatus.(exit ~msg Invalid_flowconfig)

  let main
      error_flags
      include_suppressed
      json
      pretty
      log_file
      wait
      lazy_
      debug
      profile
      all
      weak
      traces
      lib
      no_flowlib
      munge_underscore_members
      max_workers
      ignore_version
      flowconfig_flags
      verbose
      strip_root
      temp_dir
      shm_flags
      from
      quiet
      path_opt
      () =

    (* initialize loggers *)
    FlowEventLogger.set_from from;
    Hh_logger.Level.set_filter (
      if quiet then (function _ -> false)
      else if verbose != None || debug then (function _ -> true)
      else Config.default_log_filter
    );

    let root = CommandUtils.guess_root path_opt in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in

    begin match ignore_version, FlowConfig.required_version flowconfig with
    | false, Some version -> assert_version version
    | _ -> ()
    end;

    let opt_module = FlowConfig.module_system flowconfig in
    let opt_traces = match traces with
      | Some level -> level
      | None -> FlowConfig.traces flowconfig in
    let opt_munge_underscores = munge_underscore_members ||
      FlowConfig.munge_underscores flowconfig in
    let opt_temp_dir = match temp_dir with
    | Some x -> x
    | None -> FlowConfig.temp_dir flowconfig
    in
    let opt_temp_dir = Path.to_string (Path.make opt_temp_dir) in
    let opt_max_workers = match max_workers with
    | Some x -> x
    | None -> FlowConfig.max_workers flowconfig
    in
    let all = all || FlowConfig.all flowconfig in
    let weak = weak || FlowConfig.weak flowconfig in
    let opt_max_workers = min opt_max_workers Sys_utils.nbr_procs in

    let shared_mem_config = shm_config shm_flags flowconfig in

    let file_options = CommandUtils.file_options
      ~root ~no_flowlib ~temp_dir:opt_temp_dir ~lib flowconfig_flags flowconfig in

    let options = { Options.
      opt_focus_check_target =
        Config.(if mode = FocusCheck
          then Option.find_map path_opt ~f:(fun file ->
            Some (Loc.SourceFile Path.(to_string (make file))))
          else None);
      opt_lazy = lazy_;
      opt_root = root;
      opt_debug = debug;
      opt_verbose = verbose;
      opt_all = all;
      opt_weak = weak;
      opt_traces;
      opt_quiet = quiet || json || pretty;
      opt_module_name_mappers = FlowConfig.module_name_mappers flowconfig;
      opt_modules_are_use_strict = FlowConfig.modules_are_use_strict flowconfig;
      opt_output_graphml = false;
      opt_profile = profile;
      opt_strip_root = strip_root;
      opt_module;
      opt_munge_underscores = opt_munge_underscores;
      opt_temp_dir;
      opt_max_workers;
      opt_suppress_comments = FlowConfig.suppress_comments flowconfig;
      opt_suppress_types = FlowConfig.suppress_types flowconfig;
      opt_enable_const_params = FlowConfig.enable_const_params flowconfig;
      opt_enforce_strict_type_args = FlowConfig.enforce_strict_type_args flowconfig;
      opt_enforce_strict_call_arity = FlowConfig.enforce_strict_call_arity flowconfig;
      opt_enable_unsafe_getters_and_setters = FlowConfig.enable_unsafe_getters_and_setters flowconfig;
      opt_esproposal_decorators = FlowConfig.esproposal_decorators flowconfig;
      opt_esproposal_export_star_as = FlowConfig.esproposal_export_star_as flowconfig;
      opt_facebook_fbt = FlowConfig.facebook_fbt flowconfig;
      opt_ignore_non_literal_requires = FlowConfig.ignore_non_literal_requires flowconfig;
      opt_esproposal_class_static_fields = FlowConfig.esproposal_class_static_fields flowconfig;
      opt_esproposal_class_instance_fields = FlowConfig.esproposal_class_instance_fields flowconfig;
      opt_max_header_tokens = FlowConfig.max_header_tokens flowconfig;
      opt_haste_name_reducers = FlowConfig.haste_name_reducers flowconfig;
      opt_haste_paths_blacklist = FlowConfig.haste_paths_blacklist flowconfig;
      opt_haste_paths_whitelist = FlowConfig.haste_paths_whitelist flowconfig;
      opt_haste_use_name_reducers = FlowConfig.haste_use_name_reducers flowconfig;
      opt_file_options = file_options;
    } in
    match Config.mode with
    | Start ->
      let log_file = match log_file with
        | Some s ->
            let dirname = Path.make (Filename.dirname s) in
            let basename = Filename.basename s in
            Path.concat dirname basename
        | None ->
            CommandUtils.log_file ~tmp_dir:opt_temp_dir root flowconfig
      in
      let log_file = Path.to_string log_file in
      let on_spawn pid =
        if pretty || json then begin
          let open Hh_json in
          let json = json_to_string ~pretty (JSON_Object [
            "pid", JSON_String (string_of_int pid);
            "log_file", JSON_String log_file;
          ]) in
          print_endline json
        end else if not (Options.is_quiet options) then begin
          Printf.eprintf
            "Spawned flow server (pid=%d)\n" pid;
          Printf.eprintf
            "Logs will go to %s\n%!" log_file
        end
      in
      Main.daemonize ~wait ~log_file ~shared_mem_config ~on_spawn options
    | Server -> Main.run ~shared_mem_config options
    (* NOTE: At this experimental stage, focus mode implies check mode, so that
       we kill the server after we are done. Later on, focus mode might keep the
       server running after we are done. *)
    | Check | FocusCheck ->
      let profiling, errors, suppressed_errors = Main.check_once
        ~shared_mem_config options in
      let suppressed_errors =
        if include_suppressed then suppressed_errors else [] in
      let printer =
        if json || pretty then Json { pretty } else Cli error_flags in
      print_errors ~printer ~profiling ~suppressed_errors options errors;
      if Errors.ErrorSet.is_empty errors
        then FlowExitStatus.(exit No_error)
        else FlowExitStatus.(exit Type_error)

  let command = CommandSpec.command spec main
end

module CheckCommand = OptionParser (struct
  let mode = Check
  let default_log_filter = function
    | Hh_logger.Level.Fatal
    | Hh_logger.Level.Error -> true
    | _ -> false
end)
module ServerCommand = OptionParser (struct
  let mode = Server
  let default_log_filter = Hh_logger.Level.default_filter
end)
module StartCommand = OptionParser (struct
  let mode = Start
  let default_log_filter = Hh_logger.Level.default_filter
end)
module FocusCheckCommand = OptionParser (struct
  let mode = FocusCheck
  let default_log_filter = Hh_logger.Level.default_filter
end)
