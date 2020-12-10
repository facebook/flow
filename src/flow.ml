(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow shell is just a simple command-dispatching main *)
(***********************************************************************)

module FlowShell : sig
  val main : unit -> unit
end = struct
  (* normal commands *)
  let commands =
    [
      AstCommand.command;
      AutocompleteCommand.command;
      AutofixCommand.command;
      CheckCommands.CheckCommand.command;
      CheckCommands.FocusCheckCommand.command;
      CheckContentsCommand.command;
      CodemodCommand.command;
      ConfigCommand.command;
      CoverageCommand.command;
      BatchCoverageCommand.command;
      CycleCommand.command;
      GraphCommand.command;
      DumpTypesCommand.command;
      FindModuleCommand.command;
      FindRefsCommand.command;
      ForceRecheckCommand.command;
      GetDefCommand.command;
      GetImportsCommand.command;
      InitCommand.command;
      LspCommand.command;
      LsCommand.command;
      RefactorCommand.command;
      SaveStateCommand.command;
      ServerCommand.command;
      StartCommand.command;
      StopCommand.command;
      SuggestCommand.command;
      TypeAtPosCommand.command;
      VersionCommand.command;
    ]
    @ Extra_commands.extra_commands ()

  (* status commands, which need a list of other commands *)
  module StatusCommand = StatusCommands.Status (struct
    let commands = commands
  end)

  let commands = StatusCommand.command :: commands

  module DefaultCommand = StatusCommands.Default (struct
    let commands = commands
  end)

  let commands = DefaultCommand.command :: commands

  module ShellCommand = ShellCompleteCommand.Command (struct
    let commands = commands
  end)

  let commands = ShellCommand.command :: commands

  let main () =
    let default_command = DefaultCommand.command in
    let argv = Array.to_list Sys.argv in
    let (command, argv) =
      match argv with
      | [] -> failwith "Expected command"
      | [_cmd] -> (default_command, [])
      | _cmd :: next :: rest ->
        let subcmd = String.lowercase_ascii next in
        (try
           let command = List.find (fun command -> CommandSpec.name command = subcmd) commands in
           (command, rest)
         with Not_found -> (default_command, next :: rest))
    in

    let command_string = CommandSpec.name command in
    FlowEventLogger.set_command (Some command_string);

    let init_id = Random_id.short_string () in
    FlowEventLogger.init_flow_command ~init_id ~version:Flow_version.version;

    CommandUtils.run_command command argv
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
  let () = Exception.record_backtrace true in
  let () = if Sys_utils.get_env "IN_FLOW_TEST" <> None then LoggingUtils.disable_logging () in
  try
    Daemon.check_entry_point ();

    (* this call might not return *)
    FlowShell.main ()
  with
  | SharedMem.Out_of_shared_memory as e ->
    let e = Exception.wrap e in
    let bt = Exception.get_backtrace_string e in
    let msg =
      Utils.spf
        "Out of shared memory%s"
        ( if bt = "" then
          bt
        else
          ":\n" ^ bt )
    in
    FlowExitStatus.(exit ~msg Out_of_shared_memory)
  | e ->
    let e = Exception.wrap e in
    let msg = Utils.spf "Unhandled exception: %s" (Exception.to_string e) in
    FlowExitStatus.(exit ~msg Unknown_error)

(* If we haven't exited yet, let's exit now for logging's sake *)
let _ = FlowExitStatus.(exit No_error)
