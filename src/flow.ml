(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow shell is just a simple command-dispatching main *)
(***********************************************************************)

module FlowShell : sig
  val main : unit -> unit
end = struct

  (* normal commands *)
  let commands = [
    AstCommand.command;
    AutocompleteCommand.command;
    ServerCommands.Check.command;
    CheckContentsCommand.command;
    ConvertCommand.command;
    CoverageCommand.command;
    FindModuleCommand.command;
    GetDefCommand.command;
    GetImportersCommand.command;
    GetImportsCommand.command;
    ConfigCommands.Init.command;
    PortCommand.command;
    ServerCommands.Server.command;
    SearchCommand.command;
    ServerCommands.Start.command;
    StopCommand.command;
    SuggestCommand.command;
    TypeAtPosCommand.command;
    DumpTypesCommand.command;
    VersionCommand.command;
  ]

  (* status commands, which need a list of other commands *)
  module StatusCommand = StatusCommands.Status(struct
    let commands = commands
  end)
  let commands = StatusCommand.command :: commands

  module DefaultCommand = StatusCommands.Default(struct
    let commands = commands
  end)
  let commands = DefaultCommand.command :: commands

  module ShellCommand = ShellCompleteCommand.Command(struct
    let commands = commands
  end)
  let commands = ShellCommand.command :: commands

  let main () =
    Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
    let default_command = DefaultCommand.command in
    let argv = Array.to_list Sys.argv in
    let default = CommandSpec.name default_command in
    let (command, argv) = match argv with
    | [] -> failwith "Expected command"
    | cmd::[] -> (default_command, cmd::default::[])
    | cmd::next::rest ->
        let subcmd = String.lowercase next in
        try
          let command = List.find (fun command ->
            (CommandSpec.name command) = subcmd
          ) commands in
          (command, argv)
        with Not_found ->
          (default_command, cmd::default::next::rest)
    in
    let command_string = CommandSpec.name command in
    FlowEventLogger.set_command (Some command_string);
    FlowEventLogger.init_flow_command ~version:FlowConfig.version;
    CommandSpec.run command argv

end

let () =
  Daemon.check_entry_point (); (* this call might not return *)
  FlowShell.main ();
  (* If we haven't exited yet, let's exit now for logging's sake *)
  FlowExitStatus.(exit Ok)
