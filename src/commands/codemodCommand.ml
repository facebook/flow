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

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let usage = Printf.sprintf "Usage: %s codemod SUBCOMMAND [OPTIONS]...\n" CommandUtils.exe_name in
  let args =
    let open CommandSpec.ArgSpec in
    let subcommands = command [] in
    empty |> anon "subcommand" (required subcommands)
  in
  let spec =
    (* TODO when the command is ready make it available in the help menu by adding
       "Runs large-scale codebase refactors" to `doc`. *)
    { CommandSpec.name = "codemod"; doc = ""; usage; args }
  in
  CommandSpec.command spec main
