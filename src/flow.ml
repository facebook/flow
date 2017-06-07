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
(* flow shell is just a simple command-dispatching main *)
(***********************************************************************)

module FlowShell : sig
  val main : unit -> unit
end = struct

  (* normal commands *)
  let commands = [
    AstCommand.command;
    AutocompleteCommand.command;
    CheckCommands.CheckCommand.command;
    CheckCommands.FocusCheckCommand.command;
    CheckContentsCommand.command;
    ConfigCommands.Init.command;
    ConvertCommand.command;
    CoverageCommand.command;
    DumpTypesCommand.command;
    FindModuleCommand.command;
    FindRefsCommand.command;
    ForceRecheckCommand.command;
    GenFlowFilesCommand.command;
    GetDefCommand.command;
    GetImportsCommand.command;
    IdeCommand.command;
    LsCommand.command;
    PortCommand.command;
    ServerCommand.command;
    StartCommand.command;
    StopCommand.command;
    SuggestCommand.command;
    TypeAtPosCommand.command;
    VersionCommand.command;
  ] @ (Extra_commands.extra_commands ())

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
    let default_command = DefaultCommand.command in
    let argv = Array.to_list Sys.argv in
    let (command, argv) = match argv with
    | [] -> failwith "Expected command"
    | _cmd::[] -> (default_command, [])
    | _cmd::next::rest ->
        let subcmd = String.lowercase_ascii next in
        try
          let command = List.find (fun command ->
            (CommandSpec.name command) = subcmd
          ) commands in
          (command, rest)
        with Not_found ->
          (default_command, next::rest)
    in
    let command_string = CommandSpec.name command in
    FlowEventLogger.set_command (Some command_string);
    FlowEventLogger.init_flow_command ~version:Flow_version.version;

    let args =
      try CommandSpec.args_of_argv command argv
      with CommandSpec.Failed_to_parse msg ->
        let msg = Utils_js.spf
          "%s: %s\n%s"
          (Filename.basename Sys.executable_name)
          msg
          (CommandSpec.string_of_usage command)
        in
        FlowExitStatus.(exit ~msg Commandline_usage_error)
    in

    try CommandSpec.run command args
    with CommandSpec.Show_help ->
      print_endline (CommandSpec.string_of_usage command);
      FlowExitStatus.(exit No_error)
end

let _ =
  (* A SIGPIPE signal happens when writing to a closed pipe (e.g. C-c, or piping to `head` which
     exits after it prints enough lines). By default, SIGPIPE kills the program because this is a
     sane behavior for most commands; it makes them stop processing input if they can't write it
     anywhere.

     We don't like being killed uncleanly like that. By ignoring SIGPIPE, the write() syscall that
     normally would cause a SIGPIPE instead throws an EPIPE exception. We handle exceptions and
     exit via FlowExitStatus.exit instead. *)
  let () = Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore in

  try
    Daemon.check_entry_point (); (* this call might not return *)
    FlowShell.main ()
  with
  | SharedMem_js.Out_of_shared_memory ->
      FlowExitStatus.(exit Out_of_shared_memory)
  | e ->
      let bt = Printexc.get_backtrace () in
      let msg = Utils.spf "Unhandled exception: %s%s"
        (Printexc.to_string e)
        (if bt = "" then bt else "\n"^bt)
      in
      FlowExitStatus.(exit ~msg Unknown_error)

(* If we haven't exited yet, let's exit now for logging's sake *)
let _ = FlowExitStatus.(exit No_error)
