(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open CommandInfo

(***********************************************************************)
(* flow status (report current error set) command impl *)
(***********************************************************************)

module type CONFIG = sig
  (* explicit == called with "flow status ..."
     rather than simply "flow ..." *)
  val explicit : bool
end

module Impl (CommandList : COMMAND_LIST) (Config : CONFIG) = struct

  let spec = if Config.explicit
  then
    {
      CommandSpec.
      name = "status";
      doc = "(default) Shows current Flow errors by asking the Flow server";
      usage = Printf.sprintf
        "Usage: %s status [OPTION]... [ROOT]\n\
          Shows current Flow errors by asking the Flow server.\n\n\
          Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
          ROOT is assumed to be the current directory if unspecified.\n\
          A server will be started if none is running over ROOT.\n\
          \n\
          Status command options:"
          CommandUtils.exe_name;
      args = CommandSpec.ArgSpec.(
        empty
        |> CommandUtils.server_flags
        |> CommandUtils.json_flags
        |> CommandUtils.error_flags
        |> CommandUtils.strip_root_flag
        |> dummy false (* match --version below *)
        |> anon "root" (optional string) ~doc:"Root directory"
      )
    }
  else
    let command_info = CommandList.commands
      |> List.map (fun (command) ->
        (CommandSpec.name command, CommandSpec.doc command)
      )
      |> List.filter (fun (cmd, doc) -> cmd <> "" && doc <> "")
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    let col_width = List.fold_left
      (fun acc (cmd, _) -> max acc (String.length cmd)) 0 command_info in
    let cmd_usage = command_info
      |> List.map (fun (cmd, doc) ->
            Utils_js.spf "  %-*s  %s" col_width cmd doc
         )
      |> String.concat "\n"
    in
    {
      CommandSpec.
      name = "default";
      doc = "";
      usage = Printf.sprintf
        "Usage: %s [COMMAND] \n\n\
          Valid values for COMMAND:\n%s\n\n\
          Default values if unspecified:\n\
            \ \ COMMAND\
              \tstatus\n\
          \n\
          Status command options:"
          CommandUtils.exe_name
          cmd_usage;
      args = CommandSpec.ArgSpec.(
        empty
        |> CommandUtils.server_flags
        |> CommandUtils.json_flags
        |> CommandUtils.error_flags
        |> CommandUtils.strip_root_flag
        |> flag "--version" no_arg
            ~doc:"(Deprecated, use `flow version` instead) Print version number and exit"
        |> anon "root" (optional string) ~doc:"Root directory"
      )
    }

  type args = {
    root: Path.t;
    from: string;
    output_json: bool;
    pretty: bool;
    error_flags: Options.error_flags;
    strip_root: bool;
  }

  let rec check_status (args:args) server_flags =
    let name = "flow" in

    let ic, oc = CommandUtils.connect server_flags args.root in
    ServerProt.cmd_to_channel oc (ServerProt.STATUS args.root);
    let response = ServerProt.response_from_channel ic in
    let strip_root = args.strip_root in
    let print_json =
      Errors.print_error_json ~strip_root ~root:args.root ~pretty:args.pretty
    in
    match response with
    | ServerProt.DIRECTORY_MISMATCH d ->
      Printf.printf "%s is running on a different directory.\n" name;
      Printf.printf "server_root: %s, client_root: %s\n"
        (Path.to_string d.ServerProt.server)
        (Path.to_string d.ServerProt.client);
      flush stdout;
      raise CommandExceptions.Server_directory_mismatch
    | ServerProt.ERRORS errors ->
      let error_flags = args.error_flags in
      begin if args.output_json then
        print_json stdout errors
      else if args.from = "vim" || args.from = "emacs" then
        Errors.print_error_deprecated ~strip_root ~root:args.root stdout errors
      else
        Errors.print_error_summary ~strip_root ~flags:error_flags ~root:args.root errors
      end;
      FlowExitStatus.(exit Type_error)
    | ServerProt.NO_ERRORS ->
      if args.output_json
      then print_json stdout []
      else Printf.printf "No errors!\n%!";
      FlowExitStatus.(exit No_error)
    | ServerProt.NOT_COVERED ->
      let msg = "Why on earth did the server respond with NOT_COVERED?" in
      FlowExitStatus.(exit ~msg Unknown_error)
    | ServerProt.PONG ->
      let msg = "Why on earth did the server respond with a pong?" in
      FlowExitStatus.(exit ~msg Unknown_error)
    | ServerProt.SERVER_DYING ->
      let msg = Utils_js.spf
        "Server has been killed for %s"
        (Path.to_string args.root) in
      FlowExitStatus.(exit ~msg Server_dying)
    | ServerProt.SERVER_OUT_OF_DATE ->
      if server_flags.CommandUtils.no_auto_start
      then Printf.printf "%s is outdated, killing it.\n" name
      else Printf.printf "%s is outdated, going to launch a new one.\n" name;
      flush stdout;
      retry (args, server_flags) 3 "The flow server will be ready in a moment"


  and retry (args, server_flags) sleep msg =
    CommandUtils.check_timeout ();
    let retries = server_flags.CommandUtils.retries in
    if retries > 0
    then begin
      Printf.fprintf stderr "%s\n%!" msg;
      CommandUtils.sleep sleep;
      check_status args { server_flags with CommandUtils.retries = retries - 1 }
    end else
      FlowExitStatus.(exit ~msg:"Out of retries, exiting!" Out_of_retries)

  let main server_flags json pretty error_flags strip_root version root () =
    if version then (
      prerr_endline "Warning: \
        `flow --version` is deprecated in favor of `flow version`";
      CommandUtils.print_version ();
      FlowExitStatus.(exit No_error)
    );

    let root = CommandUtils.guess_root root in
    let timeout_arg = server_flags.CommandUtils.timeout in
    if timeout_arg > 0 then CommandUtils.set_timeout timeout_arg;

    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
    let strip_root = strip_root || FlowConfig.(flowconfig.options.Opts.strip_root) in

    let json = json || pretty in

    let args = {
      root;
      from = server_flags.CommandUtils.from;
      output_json = json;
      pretty;
      error_flags;
      strip_root;
    } in
    check_status args server_flags
end

module Status(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = true end)
  let command = CommandSpec.command Main.spec Main.main
end

module Default(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = false end)
  let command = CommandSpec.command Main.spec Main.main
end
