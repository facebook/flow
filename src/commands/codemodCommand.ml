(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameMap = Utils_js.FilenameMap

let environment_initialized = ref None

let initialize_environment () =
  if !environment_initialized = None then (
    (* Kickstart daemon processes *)
    Daemon.check_entry_point ();

    (* Disable monitor updates as this is a single-use tool *)
    MonitorRPC.disable ();

    (* Improve backtraces *)
    Exception.record_backtrace true;

    (* Mark the environment as initialized *)
    environment_initialized := Some ()
  ) else
    ()

let main config codemod_flags () =
  let (CommandUtils.Codemod_params
        {
          options_flags = option_values;
          shm_flags;
          info;
          verbose;
          dry_run;
          log_level;
          root;
          input_file;
          base_flag = base_flags;
          anon = filenames;
        }) =
    codemod_flags
  in
  initialize_environment ();
  let filenames = CommandUtils.get_filenames_from_input input_file filenames in
  let flowconfig_name = base_flags.CommandUtils.Base_flags.flowconfig_name in
  let root =
    match root with
    | None -> CommandUtils.guess_root flowconfig_name (Some (List.hd filenames))
    | Some provided_root ->
      let dir = Path.make provided_root in
      if Path.file_exists (Path.concat dir flowconfig_name) then
        dir
      else
        let msg = Utils_js.spf "Invalid root directory %s" provided_root in
        FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
  in
  let flowconfig =
    CommandUtils.read_config_or_exit (Server_files_js.config_file flowconfig_name root)
  in
  let shared_mem_config = CommandUtils.shm_config shm_flags flowconfig in
  let options =
    CommandUtils.make_options
      ~flowconfig_name
      ~flowconfig
      ~lazy_mode:(Some Options.LAZY_MODE_FILESYSTEM)
      ~root
      ~file_watcher_timeout:None
      option_values
  in
  (* Normalizes filepaths (symlinks and shortcuts) *)
  if filenames = [] then (
    Printf.eprintf "Error: filenames or --input-file are required\n%!";
    exit 64 (* EX_USAGE *)
  );
  let file_options = Options.file_options options in

  let roots = CommandUtils.expand_file_list ~options:file_options filenames in
  Codemod_utils.mk_main config ~options ~info ~verbose ~dry_run ~log_level ~shared_mem_config roots

(***********************)
(* Codemod subcommands *)
(***********************)

module Annotate_exports_command = struct
  let doc =
    "Annotates parts of input that are visible from the exports as required by Flow types-first mode."

  let spec =
    {
      CommandSpec.name = "annotate-exports";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-exports [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.codemod_flags
          |> flag
               "--preserve-literals"
               no_arg
               ~doc:
                 ( "Always add precise literal types for string and numeric literals. "
                 ^ "If not set the codemod uses a heuristic to widen to the respective general type."
                 )
          |> flag
               "--iteration"
               (required ~default:0 int)
               ~doc:"Include this number in imported names if greater than 0."
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allows in a single type annotation. (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails");
    }

  let config preserve_literals iteration max_type_size default_any =
    let open Codemod_utils in
    let runner =
      TypedRunner
        (Mapper (Annotate_exports.mapper ~preserve_literals ~iteration ~max_type_size ~default_any))
    in
    { reporter = Codemod_report.unit_reporter; runner }

  let main codemod_flags preserve_literals iteration max_type_size default_any () =
    let config = config preserve_literals iteration max_type_size default_any in
    main config codemod_flags ()

  let command = CommandSpec.command spec main
end

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    CommandUtils.subcommand_spec
      ~name:"codemod"
      ~doc:"Runs large-scale codebase refactors"
      [(Annotate_exports_command.spec.CommandSpec.name, Annotate_exports_command.command)]
  in
  CommandSpec.command spec main
