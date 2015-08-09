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
(* flow shell is just a simple command-dispatching main *)
(***********************************************************************)

module FlowShell : sig
  val main : unit -> unit
end = struct
  type command =
    | AUTOCOMPLETE
    | CHECK
    | CHECK_CONTENTS
    | CONFIG_INIT
    | CONVERT
    | FIND_MODULE
    | GET_DEF
    | GET_IMPORTERS
    | GET_IMPORTS
    | PORT
    | SERVER
    | SINGLE
    | START
    | STATUS
    | STOP
    | SUGGEST
    | TYPE_AT_POS
    | DEFAULT

  (* normal commands *)
  let commands = [
    AUTOCOMPLETE,   (module AutocompleteCommand : COMMAND);
    CHECK,          (module ServerCommands.Check);
    CHECK_CONTENTS, (module CheckContentsCommand);
    CONVERT,        (module ConvertCommand);
    FIND_MODULE,    (module FindModuleCommand);
    GET_DEF,        (module GetDefCommand);
    GET_IMPORTERS,  (module GetImportersCommand);
    GET_IMPORTS,    (module GetImportsCommand);
    CONFIG_INIT,    (module ConfigCommands.Init);
    PORT,           (module PortCommand);
    SERVER,         (module ServerCommands.Server);
    SINGLE,         (module SingleCommand);
    START,          (module ServerCommands.Start);
    STOP,           (module StopCommand);
    SUGGEST,        (module SuggestCommand);
    TYPE_AT_POS,    (module TypeAtPosCommand);
  ]

  (* status commands, which need a list of other commands *)
  let commands =
    (STATUS, (
      module StatusCommands.Status(struct
        let commands = mk_list commands
      end) : COMMAND
    )) :: commands

  let commands =
    (DEFAULT, (
      module StatusCommands.Default(struct
        let commands = mk_list commands
      end) : COMMAND)
    ) :: commands


  let main () =
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let (command, command_string) = parse_command commands in
    let command = (match command with None -> DEFAULT | Some cmd -> cmd) in
    FlowEventLogger.init_flow_command command_string;
    let the_module = get_command_module commands command in
    let module Command = (val the_module : COMMAND) in
    Command.run ()

end

let _ = FlowShell.main ()
