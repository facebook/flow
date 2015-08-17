(**
 * Copyright (c) 2014, Facebook, Inc.
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
            Utils.spf "  %-*s  %s" col_width cmd doc
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
        |> flag "--version" no_arg
            ~doc:"Print version number and exit"
        |> anon "root" (optional string) ~doc:"Root directory"
      )
    }

  type env = {
    root: Path.t;
    from: string;
    output_json: bool;
    error_flags: Errors_js.flags;
  }

  let rec check_status (args:env) server_flags =
    let name = "flow" in

    let ic, oc = CommandUtils.connect_with_autostart server_flags args.root in
    ServerProt.cmd_to_channel oc (ServerProt.STATUS args.root);
    let response = ServerProt.response_from_channel ic in
    match response with
    | ServerProt.DIRECTORY_MISMATCH d ->
      Printf.printf "%s is running on a different directory.\n" name;
      Printf.printf "server_root: %s, client_root: %s\n"
        (Path.to_string d.ServerProt.server)
        (Path.to_string d.ServerProt.client);
      flush stdout;
      raise CommandExceptions.Server_directory_mismatch
    | ServerProt.ERRORS e ->
      let error_flags = args.error_flags in
      begin if args.output_json then
        Errors_js.print_error_json stdout e
      else if args.from = "vim" || args.from = "emacs" then
        Errors_js.print_error_deprecated stdout e
      else
        Errors_js.print_error_summary ~flags:error_flags e
      end;
      exit 2
    | ServerProt.NO_ERRORS ->
      if args.output_json
      then Errors_js.print_error_json stdout []
      else Printf.printf "No errors!\n%!";
      exit 0
    | ServerProt.PONG ->
        Printf.printf "Why on earth did the server respond with a pong?\n%!";
        exit 2
    | ServerProt.SERVER_DYING ->
      Printf.printf "Server has been killed for %s\n"
        (Path.to_string args.root);
      exit 2
    | ServerProt.SERVER_OUT_OF_DATE ->
      if server_flags.CommandUtils.no_auto_start
      then Printf.printf "%s is outdated, killing it.\n" name
      else Printf.printf "%s is outdated, going to launch a new one.\n" name;
      flush stdout;
      retry (args, server_flags) 3 "The flow server will be ready in a moment"


  and retry (env, server_flags) sleep msg =
    CommandUtils.check_timeout ();
    let retries = server_flags.CommandUtils.retries in
    if retries > 0
    then begin
      Printf.fprintf stderr "%s\n%!" msg;
      CommandUtils.sleep sleep;
      check_status env { server_flags with CommandUtils.retries = retries - 1 }
    end else begin
      Printf.fprintf stderr "Out of retries, exiting!\n%!";
      exit 2
    end

  let main server_flags json error_flags version root () =
    if version then (
      CommandUtils.print_version ();
      exit 0
    );

    let root = CommandUtils.guess_root root in
    let timeout_arg = server_flags.CommandUtils.timeout in
    if timeout_arg > 0 then CommandUtils.set_timeout timeout_arg;
    let env = {
      root;
      from = server_flags.CommandUtils.from;
      output_json = json;
      error_flags;
    } in
    check_status env server_flags
end

module Status(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = true end)
  let command = CommandSpec.command Main.spec Main.main
end

module Default(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = false end)
  let command = CommandSpec.command Main.spec Main.main
end
