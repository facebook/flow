(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(***********************************************************************)
(* command module type *)
(***********************************************************************)

module type COMMAND = sig
  val run : unit -> unit
end

(***********************************************************************)
(* flow shell is just a simple command-dispatching main *)
(***********************************************************************)

module FlowShell : sig
  val main : unit -> unit
end = struct
  type command =
    | AUTOCOMPLETE
    | CHECK
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

  let parse_command () : (module COMMAND) =
    let command =
      if Array.length Sys.argv < 2
      then DEFAULT
      else match String.lowercase Sys.argv.(1) with
      | "autocomplete"  -> AUTOCOMPLETE
      | "check"         -> CHECK
      | "convert"       -> CONVERT
      | "find-module"   -> FIND_MODULE
      | "get-def"       -> GET_DEF
      | "get-importers" -> GET_IMPORTERS
      | "get-imports"   -> GET_IMPORTS
      | "init"          -> CONFIG_INIT
      | "port"          -> PORT
      | "server"        -> SERVER
      | "single"        -> SINGLE
      | "start"         -> START
      | "status"        -> STATUS
      | "stop"          -> STOP
      | "suggest"       -> SUGGEST
      | "type-at-pos"   -> TYPE_AT_POS
      | _               -> DEFAULT in
    let command_string =
      if command = DEFAULT
      then "default"
      else String.lowercase Sys.argv.(1) in
    FlowEventLogger.init command_string;
    match command with
    | AUTOCOMPLETE  -> (module AutocompleteCommand)
    | CHECK         -> (module ServerCommands.Check)
    | CONVERT       -> (module ConvertCommand)
    | FIND_MODULE   -> (module FindModuleCommand)
    | GET_DEF       -> (module GetDefCommand)
    | GET_IMPORTERS -> (module GetImportersCommand)
    | GET_IMPORTS   -> (module GetImportsCommand)
    | CONFIG_INIT   -> (module ConfigCommands.Init)
    | PORT          -> (module PortCommand)
    | SERVER        -> (module ServerCommands.Server)
    | SINGLE        -> (module SingleCommand)
    | START         -> (module ServerCommands.Start)
    | STATUS        -> (module StatusCommands.Status)
    | STOP          -> (module StopCommand)
    | SUGGEST       -> (module SuggestCommand)
    | TYPE_AT_POS   -> (module TypeAtPosCommand)
    | DEFAULT       -> (module StatusCommands.Default)

  let main () =
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let module Command = (val parse_command ()) in
    Command.run ()

end

let _ = FlowShell.main ()
