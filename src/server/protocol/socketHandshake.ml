(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* When a client connects over the socket, there is a handshake. The client writes a message
 * and the Flow server monitor responds *)

type build_id = string

let build_revision =
  match Build_id.build_revision with
  | "" -> Flow_version.version
  | x -> x

(*
 * Handshake
 * 1. client sends (string*string) where fst is a JSON string that can deserialized
 *    in all cases and includes client_build_id, and snd is a Marshal.to_string that will
 *    only be deserialized if the client and server have the same build_id
 * 2. server sends back (string*string option) where fst is a JSON string that can be
 *    deserialized in all cases and says the server_build_id plus the server
 *    intent, and snd is either Some (Marshal.to_string _) if the server is sure
 *    that the client will be able to deserialize it thanks to build ids matching,
 *    or is None.
 * 3. Server behavior is as signalled by 'server_intent' in step 2:
 *    - server might intend to exit now (e.g. upon stop request, or mismatch)
 *    - might persist but will hang up the connection now (e.g. mismatch, initializing)
 *    - might persist and leave the connection open (e.g. ok)
 *)

type client_handshake_wire = string * string

type server_handshake_wire = string * string option

(* What to do on a version mismatch *)
type version_mismatch_strategy =
  | Always_stop_server (* Always stop the server *)
  | Stop_server_if_older (* Stop the server if it is older than the client. Otherwise send an error *)
  | Error_client

(* Don't stop the server, just send an error to the client *)

type client_to_monitor_1 = {
  client_build_id: build_id;
  (* Build_id.build_revision for the client *)
  client_version: string;
  (* Flow_version.version for the client *)
  is_stop_request: bool;
  (* are we requesting the server to stop? *)
  server_should_hangup_if_still_initializing: bool;
  version_mismatch_strategy: version_mismatch_strategy; (* What to do on a version mismatch *)
}

type server_intent =
  | Server_will_exit (* after receiving a stop request or as a result of a version mismatch *)
  | Server_will_hangup (* version mismatch but neither client nor server should restart *)
  | Server_will_continue

(* upon success *)

type monitor_to_client_1 = {
  server_build_id: build_id;
  (* Build_id.build_revision for the server *)
  server_bin: string;
  (* filepath to the server binary *)
  server_intent: server_intent;
  (* The result of the handshake *)
  server_version: string; (* Flow_version.version for the server *)
}

type client_type =
  | Ephemeral (* a client that sends a request, gets a response, and disconnects *)
  | Persistent of {
      logging_context: FlowEventLogger.logging_context;
      lsp: Lsp.Initialize.params option;
    }

type client_to_monitor_2 = { client_type: client_type }

type monitor_to_client_2 =
  | Server_has_too_many_clients
  | Server_still_initializing of (ServerStatus.status * FileWatcherStatus.status)
  | Server_ready

type client_handshake = client_to_monitor_1 * client_to_monitor_2

type server_handshake = monitor_to_client_1 * monitor_to_client_2 option

let version_mismatch_strategy_to_string = function
  | Always_stop_server -> "Always_stop_server"
  | Stop_server_if_older -> "Stop_server_if_older"
  | Error_client -> "Error_client"

let string_to_version_mismatch_strategy = function
  | "Always_stop_server" -> Always_stop_server
  | "Stop_server_if_older" -> Stop_server_if_older
  | "Error_client" -> Error_client
  | _ ->
    (* If someone adds a new strategy in the future old servers shouldn't explode. *)
    Error_client

let client_to_monitor_1__to_json (c : client_to_monitor_1) : Hh_json.json =
  Hh_json.(
    JSON_Object
      [ ("client_build_id", JSON_String c.client_build_id);
        ("is_stop_request", JSON_Bool c.is_stop_request);
        ( "server_should_hangup_if_still_initializing",
          JSON_Bool c.server_should_hangup_if_still_initializing );
        ("client_version", JSON_String c.client_version);
        ( "version_mismatch_strategy",
          JSON_String (version_mismatch_strategy_to_string c.version_mismatch_strategy) );
        (* Deprecated - sent to talk to old servers *)
        ( "server_should_exit_if_version_mismatch",
          JSON_Bool
            (match c.version_mismatch_strategy with
            | Always_stop_server -> true
            | Stop_server_if_older -> true
            (* Any server reading this field is older than this client *)
            | Error_client -> false) ) ])

let default_client_to_monitor_1 =
  {
    client_build_id = "INCOMPATIBLE";
    is_stop_request = false;
    server_should_hangup_if_still_initializing = false;
    client_version = "0.0.0";
    version_mismatch_strategy = Error_client;
  }

let json_to__client_to_monitor_1 (json : Hh_json.json) : client_to_monitor_1 =
  Hh_json_helpers.(
    let json = Some json in
    let d = default_client_to_monitor_1 in
    let client_build_id = Jget.string_d json "client_build_id" ~default:d.client_build_id in
    let is_stop_request = Jget.bool_d json "is_stop_request" ~default:d.is_stop_request in
    let server_should_hangup_if_still_initializing =
      Jget.bool_d
        json
        "server_should_hangup_if_still_initializing"
        ~default:d.server_should_hangup_if_still_initializing
    in
    let client_version = Jget.string_d json "client_version" ~default:d.client_version in
    let version_mismatch_strategy =
      match Jget.string_opt json "version_mismatch_strategy" with
      | Some strategy -> string_to_version_mismatch_strategy strategy
      | None ->
        (match Jget.bool_opt json "server_should_exit_if_version_mismatch" with
        | Some true -> Always_stop_server
        | Some false
        | None ->
          Error_client)
    in
    {
      client_build_id;
      is_stop_request;
      server_should_hangup_if_still_initializing;
      client_version;
      version_mismatch_strategy;
    })

let monitor_to_client_1__to_json (m : monitor_to_client_1) : Hh_json.json =
  Hh_json.(
    let intent_to_string intent =
      match intent with
      | Server_will_exit -> "Server_will_exit"
      | Server_will_hangup -> "Server_will_hangup"
      | Server_will_continue -> "Server_will_continue"
    in
    JSON_Object
      [ ("server_build_id", JSON_String m.server_build_id);
        ("server_bin", JSON_String m.server_bin);
        ("server_intent", JSON_String (m.server_intent |> intent_to_string));
        ("server_version", JSON_String m.server_version) ])

let json_to__monitor_to_client_1 (json : Hh_json.json) : monitor_to_client_1 =
  Hh_json_helpers.(
    let json = Some json in
    let string_to_intent s =
      match s with
      | "Server_will_exit" -> Server_will_exit
      | "Server_will_hangup" -> Server_will_hangup
      | "Server_will_continue" -> Server_will_continue
      | _ -> raise (Jget.Parse ("unknown intent " ^ s))
    in
    let server_build_id = Jget.string_exn json "server_build_id" in
    let server_bin = Jget.string_exn json "server_bin" in
    let server_intent = Jget.string_exn json "server_intent" |> string_to_intent in
    let server_version = Jget.string_d json ~default:"0.0.0" "server_version" in
    { server_build_id; server_bin; server_intent; server_version })
