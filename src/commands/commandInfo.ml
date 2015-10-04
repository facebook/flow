(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type COMMAND = sig
  val name : string
  val doc : string
  val run : unit -> unit
end

module type COMMAND_LIST = sig
  val commands : (module COMMAND) list
end

let command_of_string commands str =
  try
    let (cmd, _) = List.find (fun (_, the_module) ->
      let module Command = (val the_module : COMMAND) in
      Command.name = str
    ) commands in
    Some cmd
  with
  | Not_found -> None
  | exn -> raise exn

let parse_command commands =
  let command =
    if Array.length Sys.argv < 2
    then None
    else command_of_string commands (String.lowercase Sys.argv.(1)) in
  let command_string =
    if command = None
    then "default"
    else String.lowercase Sys.argv.(1) in
  (command, command_string)

let get_command_module commands command : (module COMMAND) =
  let (_, the_module) = List.find (fun (cmd, _) -> cmd = command) commands in
  the_module

let mk_list commands = List.map (fun (_, mod_) -> mod_) commands
