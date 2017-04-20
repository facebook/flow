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

module type ClientProtocol = sig
  val server_request_of_stdin_message: Buffered_line_reader.t -> Prot.request option
  val handle_server_response: Prot.response -> unit
end

module VeryUnstable: ClientProtocol = struct
  let jsonrpcize_message method_ json =
    Hh_json.(
      JSON_Object [
        ("jsonrpc", JSON_String "2.0");
        ("method", JSON_String method_);
        ("params", json);
      ]
    )

  let print_errors errors =
    let json_errors = Errors.Json_output.full_status_json_of_errors ~strip_root:None errors in
    let json_message = jsonrpcize_message "diagnosticsNotification" json_errors in
    let json_string = Hh_json.json_to_string json_message in
    Http_lite.write_message stdout json_string;
    prerr_endline "sent diagnostics notification"

  let print_start_recheck () =
    Hh_json.JSON_Null
      |> jsonrpcize_message "startRecheck"
      |> Hh_json.json_to_string
      |> Http_lite.write_message stdout

  let print_end_recheck () =
    Hh_json.JSON_Null
      |> jsonrpcize_message "endRecheck"
      |> Hh_json.json_to_string
      |> Http_lite.write_message stdout

  let handle_server_response = function
    | Prot.Errors errors -> print_errors errors
    | Prot.StartRecheck -> print_start_recheck ()
    | Prot.EndRecheck -> print_end_recheck ()

  let server_request_of_stdin_message buffered_stdin =
    try
      let message = Http_lite.read_message_utf8 buffered_stdin in
      let parsed = Hh_json.json_of_string message in
      let props = Hh_json.get_object_exn parsed in
      let _, method_json = List.find (function key, _ -> key = "method") props in
      let method_name = Hh_json.get_string_exn method_json in
      if method_name = "subscribeToDiagnostics" then begin
        prerr_endline "received subscribe request";
        Some Prot.Subscribe
      end else begin
        prerr_endline ("unrecognized method: " ^ method_name);
        None
      end
    with Http_lite.Malformed _ ->
      (* Currently, Nuclide sends an extra newline after each message to satisfy the implementation
      that was previously here. Now, this manifests as a malformed request after each message. Ignore
      it while we are transitioning, but still log something in case things go wrong with the real
      messages too. *)
      prerr_endline "received malformed request";
      None
end

module ProtocolFunctor (Protocol: ClientProtocol) = struct
  let handle_server_response fd =
    let (message : Prot.response) =
      try
        Marshal_tools.from_fd_with_preamble fd
      with End_of_file ->
        prerr_endline "Server closed the connection";
        (* TODO choose a standard exit code for this *)
        exit 1
    in
    Protocol.handle_server_response message

  let send_server_request fd msg =
    Marshal_tools.to_fd_with_preamble fd (msg: Prot.request)

  let handle_stdin_message buffered_stdin server_fd =
    let request_option =
      Protocol.server_request_of_stdin_message buffered_stdin
    in
    match request_option with
      | None -> ()
      | Some req -> send_server_request server_fd req

  let rec handle_all_stdin_messages buffered_stdin server_fd =
    handle_stdin_message buffered_stdin server_fd;
    if Buffered_line_reader.has_buffered_content buffered_stdin then
      handle_all_stdin_messages buffered_stdin server_fd

  let main_loop ~buffered_stdin ~ic_fd ~oc_fd =
    let stdin_fd = Buffered_line_reader.get_fd buffered_stdin in
    while true do
      (* Negative timeout means this call will wait indefinitely *)
      let readable_fds, _, _ = Unix.select [stdin_fd; ic_fd] [] [] ~-.1.0 in
      List.iter (fun fd ->
        if fd = ic_fd then begin
          handle_server_response ic_fd
        end else if fd = stdin_fd then begin
          handle_all_stdin_messages buffered_stdin oc_fd
        end else
          failwith "Internal error: select returned an unknown fd"
      ) readable_fds
    done
end

module VeryUnstableProtocol = ProtocolFunctor(VeryUnstable)

let main option_values root protocol () =
  let root = CommandUtils.guess_root root in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc ServerProt.CONNECT;
  let buffered_stdin = stdin |> Unix.descr_of_in_channel |> Buffered_line_reader.create in
  let ic_fd = Timeout.descr_of_in_channel ic in
  let oc_fd = Unix.descr_of_out_channel oc in
  let main_loop = match protocol with
    | "very-unstable" -> VeryUnstableProtocol.main_loop
    | x -> failwith ("Internal error: unknown protocol '" ^ x ^ "'")
  in
  main_loop ~buffered_stdin ~ic_fd ~oc_fd

let command = CommandSpec.command spec main
