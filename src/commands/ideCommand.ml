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

let protocol_options = ["very-unstable"]

let protocol_options_string = String.concat ", " protocol_options

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
    |> flag "--protocol" (required (enum protocol_options))
        ~doc:("Indicates the protocol to be used. One of: " ^ protocol_options_string)
    (* TODO consider using strip_root |> strip_root_flag *)
    (* TODO use this somehow? |> verbose_flags *)
  )
}

let handle_server_message fd =
  let (message : Prot.response) =
    try
      Marshal_tools.from_fd_with_preamble fd
    with End_of_file ->
      prerr_endline "Server closed the connection";
      (* TODO choose a standard exit code for this *)
      exit 1
  in
  let Prot.Errors errors = message in
  let json_errors = Errors.Json_output.full_status_json_of_errors ~strip_root:None errors in
  let json_message = Hh_json.(
    JSON_Object [
      ("jsonrpc", JSON_String "2.0");
      ("method", JSON_String "diagnosticsNotification");
      ("params", json_errors);
    ])
  in
  let json_string = Hh_json.json_to_string json_message in
  print_endline ("Content-Length: " ^ (string_of_int (String.length json_string)) ^ "\r");
  print_endline "\r";
  print_string json_string;
  flush stdout;
  prerr_endline "sent diagnostics notification"

let send_server_request fd msg =
  Marshal_tools.to_fd_with_preamble fd (msg: Prot.request)

let handle_stdin_message buffered_stdin server_fd =
  let line = Buffered_line_reader.get_next_line buffered_stdin in
  (* This is just a quick and dirty implementation that works with something very similar to
  vscode-jsonrpc. The only catch is that in this implementation we expect a line break after each
  message, which requires a small modification to the client code. This was done for expedience,
  since Buffered_line_reader operates, obviously, on lines.

  We will replace it with a full-fledged ocaml implementation of vscode-jsonrpc when it
  becomes available. *)
  (* Mostly used to trim off \r (vscode-jsonrpc is an MS thing so they use CRLF line endings) *)
  let line = String.trim line in
  if String.length line = 0 then
    (* Some lines are empty, ignore them *)
    ()
  else if String.length line > 14 && String.sub line 0 14 = "Content-Length" then
    (* Some lines are html-style headers. ignore them too *)
    ()
  else
    (* other lines hopefully actually have JSON *)
    let parsed = Hh_json.json_of_string line in
    let props = Hh_json.get_object_exn parsed in
    let _, method_json = List.find (function key, _ -> key = "method") props in
    let method_name = Hh_json.get_string_exn method_json in
    if method_name = "subscribeToDiagnostics" then begin
      prerr_endline "received subscribe request";
      send_server_request server_fd Prot.Subscribe
    end else
      prerr_endline ("unrecognized method: " ^ method_name)

let rec handle_all_stdin_messages buffered_stdin server_fd =
  handle_stdin_message buffered_stdin server_fd;
  if Buffered_line_reader.has_buffered_content buffered_stdin then
    handle_all_stdin_messages buffered_stdin server_fd

let main option_values root protocol () =
  (* Currently there is only one option, enforced by the argument parser *)
  ignore protocol;
  let root = CommandUtils.guess_root root in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc ServerProt.CONNECT;
  let buffered_stdin = stdin |> Unix.descr_of_in_channel |> Buffered_line_reader.create in
  let stdin_fd = Buffered_line_reader.get_fd buffered_stdin in
  let ic_fd = Timeout.descr_of_in_channel ic in
  let oc_fd = Unix.descr_of_out_channel oc in
  while true do
    (* Negative timeout means this call will wait indefinitely *)
    let readable_fds, _, _ = Unix.select [stdin_fd; ic_fd] [] [] ~-.1.0 in
    List.iter (fun fd ->
      if fd = ic_fd then begin
        handle_server_message ic_fd
      end else if fd = stdin_fd then begin
        handle_all_stdin_messages buffered_stdin oc_fd
      end else
        failwith "Internal error: select returned an unknown fd"
    ) readable_fds
  done

let command = CommandSpec.command spec main
