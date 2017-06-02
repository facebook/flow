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

let protocol_options = ["very-unstable"; "human-readable"]

let protocol_options_string = String.concat ", " protocol_options

let spec = {
  CommandSpec.
  name = "ide";
  doc =
    "Starts a persistent connection to the server. Currently in development and highly unstable";
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

module HumanReadable: ClientProtocol = struct
  let server_request_of_stdin_message buffered_stdin =
    let line = Buffered_line_reader.get_next_line buffered_stdin in
    let tokens = Str.split (Str.regexp "[ \t\r\n]+") line in
    match tokens with
      | ["subscribe"] -> Some Prot.Subscribe
      (* For human-readable mode (which is just for playing around, basically)
       * you need to include the file contents on the same line, and it must
       * also have the magic token. *)
      | "autocomplete"::file::contents ->
          let fileinput = File_input.FileContent (Some file, String.concat " " contents) in
          Some (Prot.Autocomplete (fileinput, 0 (* use a dummy id *)))
      | _ ->
        prerr_endline ("Command not recognized: " ^ line); None

  let handle_autocomplete = function
    | Error _ -> print_endline "Autocomplete Error"
    | Ok completions ->
        print_endline "Autocomplete results:";
        completions |>
        List.map (fun r -> r.AutocompleteService_js.res_name) |>
        List.iter (Printf.printf "  %s\n");
        flush stdout


  let handle_server_response = function
    | Prot.Errors errors ->
      let count = Errors.ErrorSet.cardinal errors in
      print_endline ("Received " ^ (string_of_int count) ^ " errors")
    | Prot.StartRecheck -> print_endline "Start recheck"
    | Prot.EndRecheck -> print_endline "End recheck"
    | Prot.AutocompleteResult (result, _ (* ignore id *)) -> handle_autocomplete result

end

module JsonRpc : sig
  type t =
    (* method name, params, id (only for requests) *)
    | Obj of (string * Hh_json.json list * int option)
    | Malformed of string
  val parse_json_rpc_response: string -> t
end = struct
  open Hh_json
  type t =
    (* method name, params, id (only for requests) *)
    | Obj of (string * json list * int option)
    | Malformed of string

  exception Malformed_exn of string

  let get_prop propname props =
    try
      List.assoc propname props
    with Not_found -> raise (Malformed_exn (propname ^ " property not found"))

  let parse_unsafe str =
    let parsed =
      try
        json_of_string str
      with Syntax_error msg -> raise (Malformed_exn msg)
    in
    let props = match parsed with
      | JSON_Object props -> props
      | _ -> raise (Malformed_exn "Message is not a JSON Object")
    in
    let method_json = get_prop "method" props in
    let params_json = get_prop "params" props in
    let id_json = try Some (List.assoc "id" props) with Not_found -> None in
    let method_name = match method_json with
      | JSON_String str -> str
      | _ -> raise (Malformed_exn "Method name is not a string")
    in
    let params = match params_json with
      (* If you don't pass any props you just get a null here *)
      | JSON_Null -> []
      | JSON_Array lst -> lst
      | _ -> raise (Malformed_exn "Unexpected params value")
    in
    let id = match id_json with
      | None -> None
      | Some (JSON_Number x) -> Some (int_of_string x)
      | Some _ -> raise (Malformed_exn "Unexpected id value")
    in
    Obj (method_name, params, id)

  let parse_json_rpc_response str =
    try
      parse_unsafe str
    with Malformed_exn msg -> Malformed msg
end

module VeryUnstable: ClientProtocol = struct
  let jsonrpcize_notification method_ json =
    Hh_json.(
      JSON_Object [
        ("jsonrpc", JSON_String "2.0");
        ("method", JSON_String method_);
        ("params", json);
      ]
    )

  let jsonrpcize_response id json =
    Hh_json.(
      JSON_Object [
        ("jsonrpc", JSON_String "2.0");
        ("id", JSON_Number (string_of_int id));
        ("result", json);
      ]
    )

  let print_errors errors =
    let json_errors = Errors.Json_output.full_status_json_of_errors
      ~strip_root:None ~suppressed_errors:([]) errors in
    let json_message = jsonrpcize_notification "diagnosticsNotification" json_errors in
    let json_string = Hh_json.json_to_string json_message in
    Http_lite.write_message stdout json_string;
    prerr_endline "sent diagnostics notification"

  let print_start_recheck () =
    Hh_json.JSON_Null
      |> jsonrpcize_notification "startRecheck"
      |> Hh_json.json_to_string
      |> Http_lite.write_message stdout

  let print_end_recheck () =
    Hh_json.JSON_Null
      |> jsonrpcize_notification "endRecheck"
      |> Hh_json.json_to_string
      |> Http_lite.write_message stdout

  let print_autocomplete response id =
    AutocompleteService_js.autocomplete_response_to_json ~strip_root:None response
      |> jsonrpcize_response id
      |> Hh_json.json_to_string
      |> Http_lite.write_message stdout

  let handle_server_response = function
    | Prot.Errors errors -> print_errors errors
    | Prot.StartRecheck -> print_start_recheck ()
    | Prot.EndRecheck -> print_end_recheck ()
    | Prot.AutocompleteResult (result, id) -> print_autocomplete result id

  let handle_autocomplete id = Hh_json.(function
    | [JSON_String file; JSON_Number line_str; JSON_Number column_str; JSON_String contents] ->
        let file = get_path_of_file file in
        let line = int_of_string line_str in
        let column = int_of_string column_str in
        let (line, column) = convert_input_pos (line, column) in
        let with_token = AutocompleteService_js.add_autocomplete_token contents line column in
        Some (Prot.Autocomplete (File_input.FileContent (Some file, with_token), id))
    | _ ->
        prerr_endline
          "Incorrect arguments passed to autocomplete. Should be filepath, line, column, contents";
        None
  )

  let server_request_of_stdin_message buffered_stdin =
    let message = try
      Some (Http_lite.read_message_utf8 buffered_stdin)
    with Http_lite.Malformed _ ->
      prerr_endline "Received a malformed http message";
      None
    in
    match message with
      | None -> None
      | Some message ->
          let obj = JsonRpc.parse_json_rpc_response message in
          match obj with
            | JsonRpc.Obj ("subscribeToDiagnostics", _, None) ->
                prerr_endline "received subscribe request";
                Some Prot.Subscribe
            | JsonRpc.Obj ("autocomplete", params, Some id) ->
                handle_autocomplete id params
            | JsonRpc.Obj (method_name, _, id) ->
                let id_str = match id with None -> "no id" | Some _ -> "an id" in
                prerr_endline
                  ("unrecognized method: " ^ method_name ^ " with " ^ id_str ^ " provided");
                None
            | JsonRpc.Malformed err ->
                prerr_endline ("Received a malformed message: " ^ err);
                None
end

module PendingRequests : sig
  type t
  val empty: t
  val add_request: t -> Prot.request -> t
  val add_response: t -> Prot.response -> t
  val ready_request: t -> (Prot.request option * t)
end = struct
  module IntMap = Map.Make(struct type t = int let compare = ( - ) end)

  type t = {
    queue: Prot.request ImmQueue.t;
    outstanding: Prot.request option;
  }

  let empty = {
    queue = ImmQueue.empty;
    outstanding = None;
  }

  let add_request t req =
    { t with
      queue = ImmQueue.push t.queue req;
    }

  let add_response t response =
    match response, t.outstanding with
      | Prot.Errors _, _
      | Prot.StartRecheck, _
      | Prot.EndRecheck, _ ->
          t
      | Prot.AutocompleteResult (_, response_id), Some (Prot.Autocomplete (_, request_id)) ->
          if response_id <> request_id then begin
            failwith "Internal error: request and response id mismatch."
          end;
          { t with outstanding = None }
      | Prot.AutocompleteResult _, Some _ ->
          failwith "Internal error: received a mismatched response type"
      | Prot.AutocompleteResult _, None ->
          failwith "Internal error: received a response when there was no outstanding request."

  let ready_request t =
    match t.outstanding with
      | Some _ -> (None, t)
      | None -> begin
          match ImmQueue.pop t.queue with
            | None, q -> (None, { t with queue = q })
            | Some req, q ->
                let outstanding = match req with
                  (* We do not expect a response from `subscribe` *)
                  | Prot.Subscribe -> None
                  | _ -> Some req
                in
                (Some req, { outstanding; queue = q })
        end
end

module ProtocolFunctor (Protocol: ClientProtocol) = struct
  let handle_server_response fd pending_requests =
    let (message : Prot.response) =
      try
        Marshal_tools.from_fd_with_preamble fd
      with End_of_file ->
        prerr_endline "Server closed the connection";
        (* TODO choose a standard exit code for this *)
        exit 1
    in
    let pending_requests = PendingRequests.add_response pending_requests message in
    Protocol.handle_server_response message;
    pending_requests

  let send_server_request fd msg =
    Marshal_tools.to_fd_with_preamble fd (msg: Prot.request)

  let handle_stdin_message buffered_stdin pending_requests =
    match Protocol.server_request_of_stdin_message buffered_stdin with
      | None -> pending_requests
      | Some req -> PendingRequests.add_request pending_requests req

  let rec handle_all_stdin_messages buffered_stdin pending_requests =
    let pending_requests = handle_stdin_message buffered_stdin pending_requests in
    if Buffered_line_reader.has_buffered_content buffered_stdin then
      handle_all_stdin_messages buffered_stdin pending_requests
    else
      pending_requests

  let rec send_pending_requests fd pending_requests =
    let (req, pending_requests) = PendingRequests.ready_request pending_requests in
    match req with
      | None -> pending_requests
      | Some req -> begin
          send_server_request fd req;
          send_pending_requests fd pending_requests
        end

  let main_loop ~buffered_stdin ~ic_fd ~oc_fd =
    let stdin_fd = Buffered_line_reader.get_fd buffered_stdin in
    let pending_requests = ref PendingRequests.empty in
    while true do
      pending_requests := send_pending_requests oc_fd !pending_requests;
      (* Negative timeout means this call will wait indefinitely *)
      let readable_fds, _, _ = Unix.select [stdin_fd; ic_fd] [] [] ~-.1.0 in
      List.iter (fun fd ->
        if fd = ic_fd then begin
          pending_requests := handle_server_response ic_fd !pending_requests
        end else if fd = stdin_fd then begin
          pending_requests := handle_all_stdin_messages buffered_stdin !pending_requests
        end else
          failwith "Internal error: select returned an unknown fd"
      ) readable_fds
    done
end

module VeryUnstableProtocol = ProtocolFunctor(VeryUnstable)
module HumanReadableProtocol = ProtocolFunctor(HumanReadable)

let main option_values root protocol () =
  let root = CommandUtils.guess_root root in
  let ic, oc = connect option_values root in
  send_command oc ServerProt.CONNECT;
  let buffered_stdin = stdin |> Unix.descr_of_in_channel |> Buffered_line_reader.create in
  let ic_fd = Timeout.descr_of_in_channel ic in
  let oc_fd = Unix.descr_of_out_channel oc in
  let main_loop = match protocol with
    | "very-unstable" -> VeryUnstableProtocol.main_loop
    | "human-readable" -> HumanReadableProtocol.main_loop
    | x -> failwith ("Internal error: unknown protocol '" ^ x ^ "'")
  in
  main_loop ~buffered_stdin ~ic_fd ~oc_fd

let command = CommandSpec.command spec main
