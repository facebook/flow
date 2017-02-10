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
(* flow ide command *)
(***********************************************************************)

open CommandUtils
module Prot = ServerProt.Persistent_connection_prot

let spec = {
  CommandSpec.
  name = "ide";
  doc = "Starts a persistent connection to the server. Currently in development and highly unstable";
  usage = Printf.sprintf
    "Usage: %s ide\n\n\
      Starts a persistent connection to the server\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    (* TODO consider using strip_root |> strip_root_flag *)
    (* TODO use this somehow? |> verbose_flags *)
  )
}

let handle_server_message fd =
  let (message : Prot.response) =
    Marshal_tools.from_fd_with_preamble fd
  in
  let Prot.Errors errors = message in
  let error_count = Errors.ErrorSet.cardinal errors in
  (* TODO we will use JSON RPC at some point in the near future. this simple message format is just
  for ease of prototyping *)
  Printf.printf "Error count: %d" error_count;
  print_newline ()

let send_server_request fd msg =
  Marshal_tools.to_fd_with_preamble fd (msg: Prot.request)

let handle_stdin_message buffered_stdin server_fd =
  let line = Buffered_line_reader.get_next_line buffered_stdin in
  (* TODO we will use JSON RPC at some point in the near future. this simple message format is just
  for ease of prototyping *)
  match line with
    | "subscribe" ->
        send_server_request server_fd Prot.Subscribe
    | _ -> print_endline ("not recognized: " ^ line)

let main option_values root () =
  let root = CommandUtils.guess_root root in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc ServerProt.CONNECT;
  let buffered_stdin = stdin |> Unix.descr_of_in_channel |> Buffered_line_reader.create in
  let stdin_fd = Buffered_line_reader.get_fd buffered_stdin in
  let ic_fd = Timeout.descr_of_in_channel ic in
  let oc_fd = Unix.descr_of_out_channel oc in
  while true do
    while Buffered_line_reader.has_buffered_content buffered_stdin do
      handle_stdin_message buffered_stdin oc_fd
    done;
    let readable_fds, _, _ = Unix.select [stdin_fd; ic_fd] [] [] 1.0 in
    match readable_fds with
      | [] -> ()
      | fd::_ ->
          if fd = ic_fd then
            handle_server_message ic_fd
          else if fd = stdin_fd then
            handle_stdin_message buffered_stdin oc_fd
          else
            failwith "Internal error: select returned an unknown fd"
  done

let command = CommandSpec.command spec main
