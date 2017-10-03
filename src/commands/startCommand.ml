(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* start command *)
(***********************************************************************)

open CommandUtils

module Main = ServerFunctors.ServerMain (Server.FlowProgram)

let spec = { CommandSpec.
  name = "start";
  doc = "Starts a Flow server";
  args = CommandSpec.ArgSpec.(
      empty
      |> options_and_json_flags
      |> log_file_flag
      |> flag "--wait" no_arg
          ~doc:"Wait for the server to finish initializing"
      |> lazy_flags
      |> shm_flags
      |> ignore_version_flag
      |> from_flag
      |> anon "root" (optional string) ~doc:"Root directory"
    );
  usage = Printf.sprintf
    "Usage: %s start [OPTION]... [ROOT]\n\n\
      Starts a Flow server.\n\n\
      Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be the current directory if unspecified.\n\
      A server will be started if none is running over ROOT.\n"
      exe_name;
}

let main
    options_flags json pretty log_file wait lazy_mode
    shm_flags ignore_version from path_opt () =

  let root = CommandUtils.guess_root path_opt in
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let options = make_options ~flowconfig ~lazy_mode ~root options_flags in

  (* initialize loggers before doing too much, especially anything that might exit *)
  LoggingUtils.init_loggers ~from ~options ();

  if not ignore_version then assert_version flowconfig;

  let shared_mem_config = shm_config shm_flags flowconfig in

  let log_file = match log_file with
    | Some s ->
        let dirname = Path.make (Filename.dirname s) in
        let basename = Filename.basename s in
        Path.concat dirname basename
    | None ->
        CommandUtils.log_file ~tmp_dir:(Options.temp_dir options) root flowconfig
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
  (* A quiet `flow start` doesn't imply a quiet `flow server` *)
  let server_options = { options with Options.opt_quiet = false } in
  Main.daemonize ~wait ~log_file ~shared_mem_config ~on_spawn server_options

let command = CommandSpec.command spec main
