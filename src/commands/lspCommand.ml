(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open CommandUtils
open Lsp
open Lsp_fmt
module List = Core_list

(***********************************************************************)
(* flow lsp command *)
(***********************************************************************)

let spec = {
  CommandSpec.
  name = "lsp";
  doc =
    "Acts as a server for the Language Server Protocol over stdin/stdout [experimental]";
  usage = Printf.sprintf
    "Usage: %s lsp\n\n\
      Runs a server for the Language Server Protocol\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> base_flags
    |> temp_dir_flag
    |> shm_flags
    |> lazy_flags
    |> autostop_flag
    |> from_flag
  )
}


(************************************************************************)
(** Protocol orchestration & helpers                                   **)
(************************************************************************)

(* LSP exit codes are specified at https://microsoft.github.io/language-server-protocol/specification#exit *)
let lsp_exit_ok () = exit 0
let lsp_exit_bad () = exit 1


(* Given an ID that came from the server, we have to wrap it when we pass it *)
(* on to the client, to encode which instance of the server it came from.    *)
(* That way, if a response comes back later from the client after the server *)
(* has died, we'll know to discard it. We wrap it as "serverid:#id" for      *)
(* numeric ids, and "serverid:'id" for strings.                              *)
type wrapped_id = { server_id: int; message_id: lsp_id; }

let encode_wrapped (wrapped_id: wrapped_id) : lsp_id =
  let {server_id; message_id;} = wrapped_id in
  match message_id with
  | NumberId id -> StringId (Printf.sprintf "%d:#%d" server_id id)
  | StringId id -> StringId (Printf.sprintf "%d:'%s" server_id id)

let decode_wrapped (lsp: lsp_id) : wrapped_id =
  let s = match lsp with
    | NumberId _ -> failwith "not a wrapped id"
    | StringId s -> s in
  let icolon = String.index s ':' in
  let server_id = int_of_string (String.sub s 0 icolon) in
  let id = String.sub s (icolon+1) ((String.length s) - icolon - 1) in
  let message_id = if (String.get s (icolon+1)) = '#'
    then NumberId (int_of_string id)
    else StringId id in
  { server_id; message_id; }

module WrappedKey = struct
  type t = wrapped_id
  let compare (x: t) (y:t) =
    if x.server_id <> y.server_id then IntKey.compare x.server_id y.server_id
    else IdKey.compare x.message_id y.message_id
end
module WrappedSet = Set.Make (WrappedKey)
module WrappedMap = MyMap.Make (WrappedKey)


type server_conn = {
  ic: Timeout.in_channel;
  oc: out_channel;
}

type show_status_t =
  | Never_shown
  | Shown of lsp_id option * ShowStatus.showStatusParams
    (* Shown (Some id, params) -- means it is currently shown *)
    (* Shown (None, params) - means it was shown but user dismissed it *)

type open_file_info = {
  (* o_open_doc is guaranteed to be up-to-date with respect to the editor *)
  o_open_doc: Lsp.TextDocumentItem.t;
  (* o_ast, if present, is guaranteed to be up-to-date. It gets computed lazily. *)
  o_ast: (Loc.t, Loc.t) Ast.program option;
  (* o_live_diagnostics, if present, is guaranteed to be up-to-date, and to only contain
   * parse errors, and to be a better source of truth about the parse errors
   * in this file than what the flow server has told us. It also gets computed lazily. *)
  o_live_diagnostics: PublishDiagnostics.diagnostic list option;
}

type initialized_env = {
  i_initialize_params: Lsp.Initialize.params;
  i_connect_params: connect_params;
  i_root: Path.t;
  i_version: string option;
  i_server_id: int;
  i_can_autostart_after_version_mismatch: bool;
  i_outstanding_local_handlers: state lsp_handler IdMap.t;
  i_outstanding_local_requests: lsp_request IdMap.t;
  i_outstanding_requests_from_server: Lsp.lsp_request WrappedMap.t;
  i_isConnected: bool; (* what we've told the client about our connection status *)
  i_status: show_status_t;
  i_open_files: open_file_info SMap.t;
  i_outstanding_diagnostics: SSet.t;
}

and disconnected_env = {
  d_ienv: initialized_env;
  d_autostart: bool;
  d_server_status: (ServerStatus.status * FileWatcherStatus.status) option;
}

and connected_env = {
  c_ienv: initialized_env;
  c_conn: server_conn;
  c_server_status: ServerStatus.status * (FileWatcherStatus.status option);
  c_recent_summaries: (float * ServerStatus.summary) list; (* newest at head of list *)
  c_about_to_exit_code: FlowExitStatus.t option;
  (* stateful handling of Errors+status from server... *)
  c_is_rechecking: bool;
  c_lazy_stats: ServerProt.Response.lazy_stats option;
  c_diagnostics: PublishDiagnostics.diagnostic list SMap.t;
  (* if server gets disconnected, we will tidy up these things... *)
  c_outstanding_requests_to_server: Lsp.IdSet.t;
  c_outstanding_diagnostics: SSet.t; (* we'll send publishDiagnostics([]) *)
}

and state =
  (* Pre_init: we haven't yet received the initialize request.           *)
  | Pre_init of connect_params
  (* Disconnected: we'll attempt to reconnect once a tick.               *)
  | Disconnected of disconnected_env
  (* Main_loop: we have a working connection to both server and client.  *)
  | Connected of connected_env
  (* Post_shutdown: we received the shutdown request.                    *)
  | Post_shutdown

exception Client_fatal_connection_exception of Marshal_tools.remote_exception_data
exception Client_recoverable_connection_exception of Marshal_tools.remote_exception_data
exception Server_fatal_connection_exception of Marshal_tools.remote_exception_data

type event =
  | Server_message of Persistent_connection_prot.response
  | Client_message of Lsp.lsp_message * Persistent_connection_prot.metadata
  | Tick (* once per second, on idle *)


let string_of_state (state: state) : string =
  match state with
  | Pre_init _ -> "Pre_init"
  | Disconnected _ -> "Disconnected"
  | Connected _ -> "Connected"
  | Post_shutdown -> "Post_shutdown"


let denorm_string_of_event (event: event) : string =
  match event with
  | Server_message response ->
    Printf.sprintf "Server_message(%s)" (Persistent_connection_prot.string_of_response response)
  | Client_message (c, _) ->
    Printf.sprintf "Client_message(%s)" (Lsp_fmt.denorm_message_to_string c)
  | Tick ->
    "Tick"

let to_stdout (json: Hh_json.json) : unit =
  (* Extra \r\n purely for easier logfile reading; not required by protocol. *)
  let s = (Hh_json.json_to_string json) ^ "\r\n\r\n" in
  Http_lite.write_message stdout s

let get_current_version flowconfig_name (root: Path.t) : string option =
  Server_files_js.config_file flowconfig_name root
    |> FlowConfig.get ~allow_cache:false
    |> FlowConfig.required_version

let get_root (state: state) : Path.t option =
  match state with
  | Connected cenv -> Some cenv.c_ienv.i_root
  | Disconnected denv -> Some denv.d_ienv.i_root
  | _ -> None

let get_open_files (state: state) : open_file_info SMap.t option =
  match state with
  | Connected cenv -> Some cenv.c_ienv.i_open_files
  | Disconnected denv -> Some denv.d_ienv.i_open_files
  | _ -> None

let update_open_file (uri: string) (open_file_info: open_file_info option) (state: state) : state =
  let update_ienv ienv =
    match open_file_info with
    | Some open_file_info -> {ienv with i_open_files=SMap.add uri open_file_info ienv.i_open_files}
    | None -> { ienv with i_open_files = SMap.remove uri ienv.i_open_files}
  in
  match state with
  | Connected cenv -> Connected { cenv with c_ienv=update_ienv cenv.c_ienv }
  | Disconnected denv -> Disconnected { denv with d_ienv=update_ienv denv.d_ienv }
  | _ -> failwith ("client shouldn't be updating files in state " ^ (string_of_state state))


let new_metadata (state: state) (message: Jsonrpc.message) : Persistent_connection_prot.metadata =
  let start_lsp_state, start_lsp_state_reason, start_server_status, start_watcher_status =
    match state with
    | Connected {c_server_status=(s,w); _} ->
      None, None, Some s, w
    | Disconnected {d_server_status=Some (s,w); _} ->
      Some (string_of_state state), None, Some s, Some w
    | Disconnected {d_server_status=None; d_ienv; _} ->
      Some (string_of_state state), Some d_ienv.i_status, None, None
    | _ ->
      Some (string_of_state state), None, None, None in
  let start_lsp_state_reason = match start_lsp_state_reason with
    | None
    | Some Never_shown -> None
    | Some (Shown (_, params)) -> Some params.ShowStatus.request.ShowMessageRequest.message
  in
  { Persistent_connection_prot.
    start_wall_time = message.Jsonrpc.timestamp;
    start_server_status;
    start_watcher_status;
    start_json_truncated = Hh_json.json_truncate message.Jsonrpc.json
      ~max_string_length:256 ~max_child_count:4;
    start_lsp_state;
    start_lsp_state_reason;
    error_info = None;
    server_profiling = None;
    client_duration = None;
    extra_data = [];
    server_logging_context = None;
  }


let get_next_event_from_server (fd: Unix.file_descr) : event =
  let r = begin try
    Server_message (Marshal_tools.from_fd_with_preamble fd)
  with e ->
    let message = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    raise (Server_fatal_connection_exception { Marshal_tools.message; stack; })
  end in
  (* The server sends an explicit 'EOF' message in case the underlying *)
  (* transport protocol doesn't result in EOF normally. We'll respond  *)
  (* to it by synthesizing the EOF exception we'd otherwise get. *)
  if r = Server_message Persistent_connection_prot.EOF then begin
    let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
    raise (Server_fatal_connection_exception { Marshal_tools.message="End_of_file"; stack; });
  end else
    r

let get_next_event_from_client
    (state: state)
    (client: Jsonrpc.queue)
    (parser: Jsonrpc.message -> Lsp.lsp_message)
  : event =
  let message = Jsonrpc.get_message client in
  match message with
  | `Message message -> Client_message (parser message, new_metadata state message)
  | `Fatal_exception edata -> raise (Client_fatal_connection_exception edata)
  | `Recoverable_exception edata -> raise (Client_recoverable_connection_exception edata)

let get_next_event
    (state: state)
    (client: Jsonrpc.queue)
    (parser: Jsonrpc.message -> Lsp.lsp_message)
  : event =
  if Jsonrpc.has_message client then
    get_next_event_from_client state client parser
  else
    let client_fd = Jsonrpc.get_read_fd client in
    match state with
    | Connected { c_conn; _ } ->
      let server_fd = Timeout.descr_of_in_channel c_conn.ic in
      let fds, _, _ = Unix.select [server_fd; client_fd] [] [] 1.0 in
      if fds = [] then Tick
      else if List.mem fds server_fd then get_next_event_from_server server_fd
      else get_next_event_from_client state client parser
    | _ ->
      let fds, _, _ = Unix.select [client_fd] [] [] 1.0 in
      if fds = [] then Tick
      else get_next_event_from_client state client parser

let show_status
    ?(titles = [])
    ?(handler = fun _title state -> state)
    ~(type_: MessageType.t)
    ~(message: string)
    ~(shortMessage: string option)
    ~(progress: int option)
    ~(total: int option)
    (ienv: initialized_env)
  : initialized_env =
  let open ShowStatus in
  let open ShowMessageRequest in
  let open MessageType in
  let use_status = Lsp_helpers.supports_status ienv.i_initialize_params in
  let actions = List.map titles ~f:(fun title -> { title; }) in
  let params = {request={type_; message; actions;}; shortMessage; progress; total;} in

  (* What should we display/hide? It's a tricky question... *)
  let will_dismiss_old, will_show_new = match use_status, ienv.i_status, params with
    (* If the new status is identical to the old, then no-op *)
    | _, Shown (_, existingParams), params when existingParams = params -> false, false
    (* If the client supports status reporting, then we'll blindly send everything *)
    | true, _, _ -> false, true
    (* If the client only supports dialog boxes, then we'll be very limited:  *)
    (* only every display failures; and if there was already an error up even *)
    (* a different one then leave it undisturbed. *)
    | false, Shown (_, {request={type_=ErrorMessage; _};_}),
        {request={type_=ErrorMessage;_};_} -> false, false
    | false, Shown (id, _), {request={type_=ErrorMessage;_};_} -> Option.is_some id, true
    | false, Shown (id, _), _ -> Option.is_some id, false
    | false, Never_shown, {request={type_=ErrorMessage;_};_} -> false, true
    | false, Never_shown, _ -> false, false in

  (* dismiss the old one *)
  let ienv = match will_dismiss_old, ienv.i_status with
    | true, Shown (id, existingParams) ->
      let id = Option.value_exn id in
      let notification = CancelRequestNotification { CancelRequest.id; } in
      let json = Lsp_fmt.print_lsp (NotificationMessage notification) in
      to_stdout json;
      { ienv with i_status = Shown (None, existingParams); }
    | _, _ -> ienv in

  (* show the new one *)
  if not will_show_new then
    ienv
  else begin
    let id = NumberId (Jsonrpc.get_next_request_id ()) in
    let request = if use_status
      then ShowStatusRequest params else ShowMessageRequestRequest params.request in
    let json = Lsp_fmt.print_lsp (RequestMessage (id, request)) in
    to_stdout json;

    let mark_ienv_shown future_ienv =
      match future_ienv.i_status with
      | Shown (Some future_id, future_params) when future_id = id ->
        { future_ienv with i_status = Shown (None, future_params); }
      | _ -> future_ienv in
    let mark_state_shown state =
      match state with
      | Connected cenv -> Connected { cenv with c_ienv = mark_ienv_shown cenv.c_ienv; }
      | Disconnected denv -> Disconnected { denv with d_ienv = mark_ienv_shown denv.d_ienv; }
      | _ -> state in
    let handle_error _e state =
      mark_state_shown state in
    let handle_result (r: ShowMessageRequest.result) state =
      let state = mark_state_shown state in
      match r with
      | Some {ShowMessageRequest.title} -> handler title state
      | None -> state in
    let handle_result = if use_status
      then (ShowStatusHandler handle_result) else (ShowMessageHandler handle_result) in
    let handlers = (handle_result, handle_error) in
    let i_outstanding_local_requests = IdMap.add id request ienv.i_outstanding_local_requests in
    let i_outstanding_local_handlers = IdMap.add id handlers ienv.i_outstanding_local_handlers
    in
    { ienv with
      i_status = Shown (Some id, params);
      i_outstanding_local_requests;
      i_outstanding_local_handlers;
    }
  end


let send_to_server (env: connected_env) (request: Persistent_connection_prot.request) : unit =
  let _bytesWritten =
    Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel env.c_conn.oc) request in
  ()


let send_lsp_to_server
    (cenv: connected_env)
    (metadata: Persistent_connection_prot.metadata)
    (message: lsp_message)
  : unit =
  send_to_server cenv (Persistent_connection_prot.LspToServer (message, metadata))


(************************************************************************)
(** Protocol                                                           **)
(************************************************************************)

let do_initialize () : Initialize.result =
  let open Initialize in
  {
    server_capabilities = {
      textDocumentSync = {
        want_openClose = true;
        want_change = IncrementalSync;
        want_willSave = false;
        want_willSaveWaitUntil = false;
        want_didSave = None;
      };
      hoverProvider = true;
      completionProvider = Some {
        resolveProvider = false;
        completion_triggerCharacters = ["."];
      };
      signatureHelpProvider = None;
      definitionProvider = true;
      referencesProvider = true;
      documentHighlightProvider = true;
      documentSymbolProvider = true;
      workspaceSymbolProvider = false;
      codeActionProvider = false;
      codeLensProvider = None;
      documentFormattingProvider = false;
      documentRangeFormattingProvider = false;
      documentOnTypeFormattingProvider = None;
      renameProvider = true;
      documentLinkProvider = None;
      executeCommandProvider = None;
      typeCoverageProvider = true;
      rageProvider = true;
    }
  }


let show_connected (env: connected_env) : state =
  (* report that we're connected to telemetry/connectionStatus *)
  let i_isConnected = Lsp_helpers.notify_connectionStatus env.c_ienv.i_initialize_params
    to_stdout env.c_ienv.i_isConnected true in
  let env = { env with c_ienv = { env.c_ienv with i_isConnected; }; } in
  (* show green status *)
  let message = "Flow server is now ready" in
  let c_ienv = show_status
    ~type_:MessageType.InfoMessage ~message ~shortMessage:None ~progress:None ~total:None env.c_ienv
  in
  Connected { env with c_ienv; }


let show_connecting (reason: CommandConnectSimple.error) (env: disconnected_env) : state =
  if reason = CommandConnectSimple.Server_missing then
    Lsp_helpers.log_info to_stdout "Starting Flow server";

  let message, shortMessage, progress, total = match reason, env.d_server_status with
    | CommandConnectSimple.Server_missing, _ -> "Flow: Server starting", None, None, None
    | CommandConnectSimple.Server_socket_missing, _ -> "Flow: Server starting?", None, None, None
    | CommandConnectSimple.Build_id_mismatch, _ -> "Flow: Server is wrong version", None, None, None
    | CommandConnectSimple.Server_busy (CommandConnectSimple.Too_many_clients), _ ->
      "Flow: Server busy", None, None, None
    | CommandConnectSimple.Server_busy _, None -> "Flow: Server busy", None, None, None
    | CommandConnectSimple.Server_busy _, Some (server_status, watcher_status) ->
      if not (ServerStatus.is_free server_status) then
        let shortMessage, progress, total = ServerStatus.get_progress server_status in
        "Flow: " ^ (ServerStatus.string_of_status ~use_emoji:true server_status),
        shortMessage, progress, total
      else
        "Flow: " ^ (FileWatcherStatus.string_of_status watcher_status), None, None, None

  in
  Disconnected { env with d_ienv = show_status
    ~type_:MessageType.WarningMessage ~message ~shortMessage ~progress ~total env.d_ienv;
  }


let show_disconnected
    (code: FlowExitStatus.t option)
    (message: string option)
    (env: disconnected_env)
  : state =
  (* report that we're disconnected to telemetry/connectionStatus *)
  let i_isConnected = Lsp_helpers.notify_connectionStatus env.d_ienv.i_initialize_params
    to_stdout env.d_ienv.i_isConnected false in
  let env = { env with d_ienv = { env.d_ienv with i_isConnected; }; } in

  (* show red status *)
  let message = Option.value message ~default:"Flow: server is stopped" in
  let message = match code with
    | Some code -> Printf.sprintf "%s [%s]" message (FlowExitStatus.to_string code)
    | None -> message in
  let handler r state = match state, r with
    | Disconnected e, "Restart" -> Disconnected { e with d_autostart = true; }
    | _ -> state
  in
  Disconnected { env with
    d_ienv = show_status ~handler ~titles:["Restart"] ~type_:MessageType.ErrorMessage
      ~message ~shortMessage:None ~progress:None ~total:None env.d_ienv;
  }


let try_connect flowconfig_name (env: disconnected_env) : state =
  (* If the version in .flowconfig has changed under our feet then we mustn't *)
  (* connect. We'll terminate and trust the editor to relaunch an ok version. *)
  let current_version = get_current_version flowconfig_name env.d_ienv.i_root in
  if env.d_ienv.i_version <> current_version then begin
    let prev_version_str = Option.value env.d_ienv.i_version ~default: "[None]" in
    let current_version_str = Option.value current_version ~default: "[None]" in
    let message =
      "\nVersion in flowconfig that spawned the existing flow server: " ^ prev_version_str ^
      "\nVersion in flowconfig currently: " ^ current_version_str ^
      "\n" in
    Lsp_helpers.telemetry_log to_stdout message;
    lsp_exit_bad ()
  end;
  let start_env = CommandUtils.make_env flowconfig_name env.d_ienv.i_connect_params
    env.d_ienv.i_root in

  let client_handshake = SocketHandshake.({
    client_build_id = build_revision;
    is_stop_request = false;
    server_should_exit_if_version_mismatch = env.d_autostart; (* only exit if we'll restart it *)
    server_should_hangup_if_still_initializing = true; }, {
    client_type = Persistent {
      logging_context = FlowEventLogger.get_context ();
      lsp = Some env.d_ienv.i_initialize_params;
    };
  }) in
  let conn = CommandConnectSimple.connect_once
    ~flowconfig_name ~client_handshake ~tmp_dir:start_env.CommandConnect.tmp_dir
    start_env.CommandConnect.root in

  match conn with
  | Ok (ic, oc) ->
    let _bytesWritten = Marshal_tools.to_fd_with_preamble
      (Unix.descr_of_out_channel oc)
      Persistent_connection_prot.Subscribe in
    let i_server_id = env.d_ienv.i_server_id + 1 in
    let new_env = {
      c_ienv = { env.d_ienv with i_server_id; };
      c_conn = { ic; oc; };
      c_server_status = (ServerStatus.initial_status, None);
      c_about_to_exit_code = None;
      c_is_rechecking = false;
      c_lazy_stats = None;
      c_diagnostics = SMap.empty;
      c_outstanding_requests_to_server = Lsp.IdSet.empty;
      c_outstanding_diagnostics = SSet.empty;
      c_recent_summaries = [];
    } in
    (* send the initial messages to the server *)
    send_to_server new_env Persistent_connection_prot.Subscribe;
    let make_open_message (textDocument: TextDocumentItem.t) : lsp_message =
      NotificationMessage (DidOpenNotification { DidOpen.textDocument; }) in
    let open_messages = env.d_ienv.i_open_files |> SMap.bindings
      |> List.map ~f:(fun (_, {o_open_doc; _}) -> make_open_message o_open_doc) in
    let open Hh_json in
    let metadata = { Persistent_connection_prot.
      start_wall_time = Unix.gettimeofday ();
      start_server_status = Some (fst new_env.c_server_status);
      start_watcher_status = snd new_env.c_server_status;
      start_json_truncated = JSON_Object ["method", JSON_String "synthetic/open"];
      start_lsp_state = None;
      start_lsp_state_reason = None;
      error_info = None;
      server_profiling = None;
      client_duration = None;
      extra_data = [];
      server_logging_context = None;
    } in
    List.iter open_messages ~f:(send_lsp_to_server new_env metadata);
    (* close the old UI and bring up the new *)
    let new_state = show_connected new_env in
    new_state

  (* Server_missing means the lock file is absent, because the server isn't running *)
  | Error (CommandConnectSimple.Server_missing as reason) ->
    let new_env = { env with d_autostart = false; d_server_status = None; } in
    if env.d_autostart then
      let start_result = CommandConnect.start_flow_server start_env in
      match start_result with
      | Ok () -> show_connecting reason new_env
      | Error (msg, code) -> show_disconnected (Some code) (Some msg) new_env
    else
      show_disconnected None None new_env

  (* Server_socket_missing means the server is present but lacks its sock *)
  (* file. There's a tiny race possibility that the server has created a  *)
  (* lock but not yet created a sock file. More likely is that the server *)
  (* is an old version of the server which doesn't even create the right  *)
  (* sock file. We'll kill the server now so we can start a new one next. *)
  (* And if it was in that race? bad luck... *)
  | Error (CommandConnectSimple.Server_socket_missing as reason) ->
    begin try
      let tmp_dir = start_env.CommandConnect.tmp_dir in
      let root = start_env.CommandConnect.root in
      CommandMeanKill.mean_kill ~flowconfig_name ~tmp_dir root;
      show_connecting reason { env with d_server_status = None; }
    with CommandMeanKill.FailedToKill _ ->
      let msg = "An old version of the Flow server is running. Please stop it." in
      show_disconnected None (Some msg) { env with d_server_status = None; }
    end

  (* Build_id_mismatch is because the server version was different from client *)
  (* If we didn't ask the server to exit on mismatch, then we're stuck.        *)
  | Error (CommandConnectSimple.Build_id_mismatch as _reason) when not env.d_autostart ->
    let msg = "Flow: the running server is the wrong version" in
    show_disconnected None (Some msg) { env with d_server_status = None; }

  (* If we did ask the server to terminate upon version mismatch, then we'll   *)
  (* just keep trying to connect, and next time we'll start a new server.      *)
  (* and the server terminates immediately after telling us this - so we'll    *)
  (* just keep trying to connect, and the next time we'll start a new server.  *)
  | Error (CommandConnectSimple.Build_id_mismatch as reason) ->
    show_connecting reason { env with d_server_status = None; }

  (* While the server is busy initializing, sometimes we get Server_busy.Fail_on_init *)
  (* with a server-status telling us how far it is through init. And sometimes we get *)
  (* just ServerStatus.Not_responding if the server was just too busy to give us a    *)
  (* status update. These are cases where the right version of the server is running  *)
  (* but it's not speaking to us just now. So we'll keep trying until it's ready.     *)
  | Error ((CommandConnectSimple.Server_busy (CommandConnectSimple.Fail_on_init st)) as reason) ->
    show_connecting reason { env with d_server_status = Some st; }

  (* The following codes mean the right version of the server is running so   *)
  (* we'll retry. They provide no information about the d_server_status of    *)
  (* the server, so we'll leave it as it was before.                          *)
  | Error ((CommandConnectSimple.Server_busy CommandConnectSimple.Not_responding) as reason)
  | Error ((CommandConnectSimple.Server_busy CommandConnectSimple.Too_many_clients) as reason) ->
    show_connecting reason env

let close_conn (env: connected_env) : unit =
  try Timeout.shutdown_connection env.c_conn.ic with _ -> ();
  try Timeout.close_in_noerr env.c_conn.ic with _ -> ()


(************************************************************************)
(** Tracking                                                           **)
(************************************************************************)
(* The goal of tracking is that, if a server goes down, then all errors *)
(* and dialogs and things it created should be taken down with it.      *)
(*                                                                      *)
(* "track_to_server" is called for client->lsp messages when they get   *)
(*   sent to the current server.                                        *)
(* "track_from_server" is called for server->lsp messages which         *)
(*   immediately get passed on to the client.                           *)
(* "dismiss_tracks" is called when a server gets disconnected.          *)
(*                                                                      *)
(* EDITOR_OPEN_FILES - we keep the current contents of all editor open  *)
(*   files. Updated in response to client->lsp notifications            *)
(*   didOpen/Change/Save/Close. When a new server starts, we synthesize *)
(*   didOpen messages to the new server.                                *)
(* OUTSTANDING_REQUESTS_TO_SERVER - for all client->lsp requests that   *)
(*   have been sent to the server. Added to this list when we           *)
(*   track_to_server(request); removed on track_from_server(response).  *)
(*   When a server dies, we synthesize RequestCancelled responses       *)
(*   ourselves since the server will no longer do that.                 *)
(* OUTSTANDING_REQUESTS_FROM_SERVER - for all server->lsp requests. We  *)
(*   generate a "wrapped-id" that encodes which server it came from,    *)
(*   and send immediately to the client. Added to this list when we     *)
(*   track_from_server(request), removed in track_to_server(response).  *)
(*   When a server dies, we emit CancelRequest notifications to the     *)
(*   client so it can dismiss dialogs or similar. When any response     *)
(*   comes back from the client, we ignore ones that are destined for   *)
(*   now-defunct servers, and only forward on the ones for the current  *)
(*   server.                                                            *)
(* OUTSTANDING_DIAGNOSTICS - for all server->lsp publishDiagnostics     *)
(*   notifications which are being displayed in the client. Added to    *)
(*   this list when we track_from_server(publishDiagnostics) a file     *)
(*   with non-empty error list; removed when we                         *)
(*   track_from_server(publishDiagnostics) a file with empty error list.*)
(*   When a server dies, we synthesize publishDiagnostics notifications *)
(*   to the client so it can erase all diagnostics.                     *)
(* OUTSTANDING_PROGRESS - for all server->lsp progress notifications    *)
(*   which are being displayed in the client. Added to this list when   *)
(*   we track_from_server(progress) a non-empty progress; removed       *)
(*   when we track_from_server(progress) an empty progress. When a      *)
(*   server dies, we synthesize progress notifications to the client    *)
(*   so it can erase all outstanding progress messages.                 *)
(* OUTSTANDING_ACTION_REQUIRED - similar to outstanding_progress.       *)

type track_effect = {
  changed_live_uri: string option;
}

let track_to_server (state: state) (c: Lsp.lsp_message) : (state * track_effect) =
  (* didOpen, didChange, didSave, didClose: save them up in editor_file_events *)
  let changed_open_file = match (get_open_files state), c with
    | _, NotificationMessage (DidOpenNotification params) ->
      let o_open_doc = params.DidOpen.textDocument in
      let uri = params.DidOpen.textDocument.TextDocumentItem.uri in
      Some (uri, Some {o_open_doc; o_ast=None; o_live_diagnostics=None;})

    | _, NotificationMessage (DidCloseNotification params) ->
      let uri = params.DidClose.textDocument.TextDocumentIdentifier.uri in
      Some (uri, None)

    | Some open_files, NotificationMessage (DidChangeNotification params) ->
      let uri = params.DidChange.textDocument.VersionedTextDocumentIdentifier.uri in
      let {o_open_doc; _} = SMap.find uri open_files in
      let text = o_open_doc.TextDocumentItem.text in
      let text = Lsp_helpers.apply_changes_unsafe text params.DidChange.contentChanges in
      let o_open_doc = { Lsp.TextDocumentItem.
        uri;
        languageId = o_open_doc.TextDocumentItem.languageId;
        version = params.DidChange.textDocument.VersionedTextDocumentIdentifier.version;
        text;
      } in
      Some (uri, Some {o_open_doc; o_ast=None; o_live_diagnostics=None;})

    | _, _ ->
      None
  in
  let state, changed_live_uri = match changed_open_file with
    | Some (uri, open_file_info) -> update_open_file uri open_file_info state, Some uri
    | None -> state, None
  in
  let state = match state, c with
    (* client->server requests *)
    | Connected env, RequestMessage (id, _) ->
      Connected { env with c_outstanding_requests_to_server =
        IdSet.add id env.c_outstanding_requests_to_server;
      }
    (* client->server responses *)
    | Connected env, ResponseMessage (id, _) ->
      let wrapped = decode_wrapped id in
      let c_ienv = { env.c_ienv with i_outstanding_requests_from_server =
        WrappedMap.remove wrapped env.c_ienv.i_outstanding_requests_from_server;
      } in
      Connected { env with c_ienv; }
    | _ -> state
  in
  state, { changed_live_uri; }


let track_from_server (state: state) (c: Lsp.lsp_message) : state =
  match state, c with
  (* server->client response *)
  | Connected env, ResponseMessage (id, _) ->
    Connected { env with c_outstanding_requests_to_server =
      IdSet.remove id env.c_outstanding_requests_to_server;
    }
  (* server->client request *)
  | Connected env, RequestMessage (id, params) ->
    let wrapped = { server_id = env.c_ienv.i_server_id; message_id = id; } in
    let c_ienv = { env.c_ienv with i_outstanding_requests_from_server =
      WrappedMap.add wrapped params env.c_ienv.i_outstanding_requests_from_server;
    } in
    Connected { env with c_ienv; }
  (* server->client publishDiagnostics: save up all URIs with non-empty diagnostics *)
  | Connected env, NotificationMessage (PublishDiagnosticsNotification params) ->
    let uri = params.PublishDiagnostics.uri in
    let published = params.PublishDiagnostics.diagnostics in
    let c_outstanding_diagnostics = match published with
      | [] -> SSet.remove uri env.c_outstanding_diagnostics
      | _ -> SSet.add uri env.c_outstanding_diagnostics in
    Connected { env with c_outstanding_diagnostics }
  | _, _ -> state

let dismiss_tracks (state: state) : state =
  let decline_request_to_server (id: lsp_id) : unit =
    let e = Error.RequestCancelled "Connection to server has been lost" in
    let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
    let json = Lsp_fmt.print_lsp_response id (ErrorResult (e, stack)) in
    to_stdout json
  in
  let cancel_request_from_server
      (server_id: int)
      (wrapped: wrapped_id)
      (_request: lsp_request): unit =
    if server_id = wrapped.server_id then
      let id = encode_wrapped wrapped in
      let notification = CancelRequestNotification { CancelRequest.id; } in
      let json = Lsp_fmt.print_lsp_notification notification in
      to_stdout json
    else
      ()
  in
  let clear_diagnostics (uri: string) : unit =
    let notification = PublishDiagnosticsNotification {
      PublishDiagnostics.uri; diagnostics = []; } in
    let json = Lsp_fmt.print_lsp_notification notification in
    to_stdout json
  in
  match state with
  | Connected env ->
    WrappedMap.iter (cancel_request_from_server env.c_ienv.i_server_id)
       env.c_ienv.i_outstanding_requests_from_server;
    IdSet.iter decline_request_to_server env.c_outstanding_requests_to_server;
    SSet.iter clear_diagnostics env.c_outstanding_diagnostics;
    Connected { env with
      c_outstanding_requests_to_server = IdSet.empty;
      c_outstanding_diagnostics = SSet.empty;
    }
  | _ -> state


let lsp_DocumentItem_to_flow (open_doc: Lsp.TextDocumentItem.t) : File_input.t =
  let uri = open_doc.TextDocumentItem.uri in
  let fn = Lsp_helpers.lsp_uri_to_path uri in
  let fn = Option.value (Sys_utils.realpath fn) ~default:fn in
  File_input.FileContent (Some fn, open_doc.TextDocumentItem.text)


let lsp_DocumentIdentifier_to_flow
    (textDocument: Lsp.TextDocumentIdentifier.t)
    ~(state: state)
  : File_input.t =
  let uri = textDocument.TextDocumentIdentifier.uri in
  let editor_open_files = get_open_files state in
  match Option.bind editor_open_files (SMap.get uri) with
  | None ->
    let fn = Lsp_helpers.lsp_uri_to_path uri in
    let fn = Option.value (Sys_utils.realpath fn) ~default:fn in
    File_input.FileName fn
  | Some {o_open_doc; _} -> lsp_DocumentItem_to_flow o_open_doc


(******************************************************************************)
(* Diagnostics                                                                *)
(* These should really be handle inside the flow server so it sends out       *)
(* LSP publishDiagnostics notifications and we track them in the normal way.  *)
(* But while the flow server has to handle legacy clients as well as LSP      *)
(* clients, we don't want to make the flow server code too complex, so we're  *)
(* handling them here for now.                                                *)
(******************************************************************************)

let error_to_lsp
    ~(severity: PublishDiagnostics.diagnosticSeverity option)
    ~(default_uri: string)
    (error: Errors.error)
  : string * PublishDiagnostics.diagnostic =
  let error = Errors.Lsp_output.lsp_of_error error in
  let location = Flow_lsp_conversions.loc_to_lsp_with_default
    error.Errors.Lsp_output.loc ~default_uri in
  let uri = location.Lsp.Location.uri in
  let related_to_lsp (loc, relatedMessage) =
    let relatedLocation = Flow_lsp_conversions.loc_to_lsp_with_default loc ~default_uri in
    { Lsp.PublishDiagnostics.relatedLocation; relatedMessage; } in
  let relatedInformation =
    List.map error.Errors.Lsp_output.relatedLocations ~f:related_to_lsp
  in
  uri, { Lsp.PublishDiagnostics.
    range = location.Lsp.Location.range;
    severity;
    code = Lsp.PublishDiagnostics.StringCode error.Errors.Lsp_output.code;
    source = Some "Flow";
    message = error.Errors.Lsp_output.message;
    relatedInformation;
    relatedLocations = relatedInformation; (* legacy fb extension *)
  }


(* parse_and_cache: either the uri is an open file for which we already
 * have parse results (ast+diagnostics), so we can just return them;
 * or it's an open file and we are expected to lazily compute the parse results
 * and store them in the state;
 * or it's an unopened file in which case we'll retrieve parse results but
 * won't store them. *)
let parse_and_cache flowconfig_name (state: state) (uri: string)
  : state * (Loc.t, Loc.t) Ast.program * PublishDiagnostics.diagnostic list option =
  (* part of parsing is producing parse errors, if so desired *)
  let liveSyntaxErrors = let open Initialize in match state with
    | Connected cenv -> cenv.c_ienv.i_initialize_params.initializationOptions.liveSyntaxErrors
    | Disconnected denv -> denv.d_ienv.i_initialize_params.initializationOptions.liveSyntaxErrors
    | _ -> false in

  let error_to_diagnostic (loc, parse_error) =
    let message = Errors.Friendly.message_of_string (Parse_error.PP.error parse_error) in
    let error = Errors.mk_error ~kind:Errors.ParseError loc message in
    let _, diagnostic = error_to_lsp
      ~default_uri:uri ~severity:(Some PublishDiagnostics.Error) error in
    diagnostic in

  (* The way flow compilation works in the flow server is that parser options *)
  (* are permissive to allow all constructs, so that parsing works well; if   *)
  (* the user choses not to enable features through the user's .flowconfig    *)
  (* then use of impermissable constructs will be reported at typecheck time  *)
  (* (not as parse errors). We'll do the same here, with permissive parsing   *)
  (* and only reporting parse errors.                                         *)
  let get_parse_options () =
    let root = get_root state in
    let use_strict = Option.value_map root ~default:false ~f:(fun root ->
      Server_files_js.config_file flowconfig_name root
      |> FlowConfig.get |> FlowConfig.modules_are_use_strict) in
    Some Parser_env.({
      esproposal_class_instance_fields = true;
      esproposal_class_static_fields = true;
      esproposal_decorators = true;
      esproposal_export_star_as = true;
      esproposal_optional_chaining = true;
      esproposal_nullish_coalescing = true;
      types = true;
      use_strict;
    }) in

  let parse file =
    let (program, errors) = try
      let content = File_input.content_of_file_input_unsafe file in
      let filename_opt = File_input.path_of_file_input file in
      let filekey = Option.map filename_opt ~f:(fun fn -> File_key.SourceFile fn) in
      let parse_options = get_parse_options () in
      Parser_flow.program_file ~fail:false ~parse_options ~token_sink:None content filekey
    with _ ->
      (Loc.none,[],[]), []
    in
    program, if liveSyntaxErrors then Some (List.map errors ~f:error_to_diagnostic) else None in

  let open_files = get_open_files state in
  let existing_open_file_info = Option.bind open_files (SMap.get uri) in
  match existing_open_file_info with
  | Some {o_ast=Some o_ast; o_live_diagnostics; _} ->
    state, o_ast, o_live_diagnostics
  | Some {o_open_doc; _} ->
    let file = lsp_DocumentItem_to_flow o_open_doc in
    let o_ast, o_live_diagnostics = parse file in
    let open_file_info = Some {o_open_doc; o_ast=Some o_ast; o_live_diagnostics;} in
    let state = update_open_file uri open_file_info state in
    state, o_ast, o_live_diagnostics
  | None ->
    let fn = Lsp_helpers.lsp_uri_to_path uri in
    let fn = Option.value (Sys_utils.realpath fn) ~default:fn in
    let file = File_input.FileName fn in
    let open_ast, open_diagnostics = parse file in
    state, open_ast, open_diagnostics


(* print_diagnostics: just pushes the set of diagnostics for this uri to the client *)
(* taking into account whether there are superceding local parse errors as well.    *)
let print_diagnostics
    (uri: string)
    (diagnostics: PublishDiagnostics.diagnostic list)
    (state: state)
  : state =
  let open PublishDiagnostics in

  let prev_server_reported, prev_open_reported = match state with
    | Connected cenv ->
      SSet.mem uri cenv.c_outstanding_diagnostics,
      SSet.mem uri cenv.c_ienv.i_outstanding_diagnostics
    | Disconnected denv ->
      false, SSet.mem uri denv.d_ienv.i_outstanding_diagnostics
    | _ -> false, false in

  (* First we'll look at server tracks, update then appropriately. *)
  let msg = NotificationMessage
    (PublishDiagnosticsNotification { PublishDiagnostics.uri; diagnostics; }) in
  let state = track_from_server state msg in

  (* Next look at open-file tracks, update them appropriately. *)
  let open_file_info = Option.bind (get_open_files state) (SMap.get uri) in
  let state, use_live, o_live_diagnostics = match open_file_info with
    | None
    | Some {o_live_diagnostics=None; _} ->
      state, false, []
    | Some {o_live_diagnostics=Some o_live_diagnostics; _} ->
      let update_ienv ienv =
        let i_outstanding_diagnostics = match o_live_diagnostics with
        | [] -> SSet.remove uri ienv.i_outstanding_diagnostics
        | _ -> SSet.add uri ienv.i_outstanding_diagnostics in
        { ienv with i_outstanding_diagnostics; } in
      let state = match state with
        | Connected cenv -> Connected {cenv with c_ienv = update_ienv cenv.c_ienv}
        | Disconnected denv -> Disconnected {denv with d_ienv=update_ienv denv.d_ienv}
        | _ -> state in
      state, true, o_live_diagnostics in

  (* If using live-diagnostics, then strip out parse errors from the server diagnostics *)
  (* and instead include the local ones. *)
  let diagnostics = if use_live then
    let parse_code = Errors.string_of_kind Errors.ParseError in
    let diagnostics = List.filter diagnostics ~f:(fun d ->  d.code <> StringCode (parse_code)) in
    o_live_diagnostics @ diagnostics
  else
    diagnostics in

  (* Avoid sending the message if it was empty before and is empty now. *)
  (* This isn't needed for correct client behavior, but it makes the transcripts *)
  (* easier to write unit-tests for! *)
  let msg = NotificationMessage
    (PublishDiagnosticsNotification { PublishDiagnostics.uri; diagnostics; }) in
  let new_reported = match diagnostics with [] -> false | _ -> true in
  if prev_open_reported || prev_server_reported || new_reported then
    to_stdout (Lsp_fmt.print_lsp msg);
  state

let do_additional_diagnostics
    (cenv: connected_env)
    (diagnostics: PublishDiagnostics.diagnostic list SMap.t)
  : state =
  (* Merge the additional diagnostics into cenv *)
  let uris = SMap.bindings diagnostics |> List.map ~f:fst |> SSet.of_list in
  let combine _uri existing additions = Some (existing @ additions) in
  let c_diagnostics = SMap.union ~combine cenv.c_diagnostics diagnostics in
  let state = Connected { cenv with c_diagnostics; } in

  (* Send publishDiagnostics for all files touched by the additions. *)
  let to_send = SMap.filter (fun uri _ -> SSet.mem uri uris) c_diagnostics in
  let state = SMap.fold print_diagnostics to_send state
  in
  state

let do_replacement_diagnostics
    (cenv: connected_env)
    (c_diagnostics: PublishDiagnostics.diagnostic list SMap.t)
  : state =
  let state = Connected { cenv with c_diagnostics; } in

  (* Send publishDiagnostics for all files that no longer have diagnostics *)
  let old_uris = SMap.bindings cenv.c_diagnostics |> List.map ~f:fst |> SSet.of_list in
  let new_uris = SMap.bindings c_diagnostics |> List.map ~f:fst |> SSet.of_list in
  let now_empty_uris = SSet.diff old_uris new_uris in
  let print_empty uri state = print_diagnostics uri [] state in
  let state = SSet.fold print_empty now_empty_uris state in

  (* Send publishDiagnostics for all files that have diagnostics *)
  let state = SMap.fold print_diagnostics c_diagnostics state
  in
  state

let do_live_diagnostics flowconfig_name (state: state) (uri: string): state =
  (* reparse the file and write it into the state's editor_open_files as needed *)
  let state, _, _ = parse_and_cache flowconfig_name state uri in
  (* republish the diagnostics for this file based on a mix of server-generated ones *)
  (* if present, and client-generated ones if the file is open *)
  let server_diagnostics = match state with
    | Connected cenv -> Option.value (SMap.get uri cenv.c_diagnostics) ~default:[]
    | _ -> [] in
  let state = print_diagnostics uri server_diagnostics state in
  state


let show_recheck_progress (cenv: connected_env) : state =
  let type_, message, shortMessage, progress, total =
  match cenv.c_is_rechecking, cenv.c_server_status, cenv.c_lazy_stats with
    | true, (server_status, _), _ when not (ServerStatus.is_free server_status) ->
      let shortMessage, progress, total = ServerStatus.get_progress server_status in
      let message = "Flow: " ^ (ServerStatus.string_of_status ~use_emoji:true server_status) in
      MessageType.WarningMessage, message, shortMessage, progress, total
    | true, _, _ ->
      MessageType.WarningMessage, "Flow: Server is rechecking...", None, None, None
    | false, _, Some {ServerProt.Response.lazy_mode=Some mode; checked_files; total_files}
      when checked_files < total_files ->
      let message = Printf.sprintf
        "Flow: done recheck. (%s lazy mode let it check only %d/%d files [[more...](%s)])"
        Options.(match mode with LAZY_MODE_FILESYSTEM -> "fs" | LAZY_MODE_IDE -> "ide")
        checked_files total_files "https://flow.org/en/docs/lang/lazy-modes/" in
      MessageType.InfoMessage, message, None, None, None
    | false, _, _ ->
      MessageType.InfoMessage, "Flow: done recheck", None, None, None
  in
  Connected { cenv with
    c_ienv = show_status~type_ ~message ~shortMessage ~progress ~total cenv.c_ienv
  }

let do_documentSymbol flowconfig_name (state: state) (id: lsp_id)
  (params: DocumentSymbol.params): state =
  let uri = params.DocumentSymbol.textDocument.TextDocumentIdentifier.uri in
  let state, ast, _ = parse_and_cache flowconfig_name state uri in
  let result = Flow_lsp_conversions.flow_ast_to_lsp_symbols ~uri ast in
  let json = Lsp_fmt.print_lsp (ResponseMessage (id, DocumentSymbolResult result)) in
  to_stdout json;
  state




module RagePrint = struct
  let addline (b: Buffer.t) (prefix: string) (s: string) : unit =
    Buffer.add_string b prefix;
    Buffer.add_string b s;
    Buffer.add_string b "\n";
    ()

  let string_of_lazy_stats (lazy_stats: ServerProt.Response.lazy_stats) : string =
    let open ServerProt in
    Printf.sprintf "lazy_mode=%s, checked_files=%d, total_files=%d"
      (Option.value_map lazy_stats.Response.lazy_mode ~default:"" ~f:Options.lazy_mode_to_string)
      lazy_stats.Response.checked_files lazy_stats.Response.total_files

  let string_of_connect_params (p: connect_params) : string =
    let open CommandUtils in
    Printf.sprintf (
      "from=%s, retries=%d, retry_if_init=%B, no_auto_start=%B, autostop=%B, \
      ignore_version=%B quiet=%B, temp_dir=%s, \
      timeout=%s, lazy_mode=%s")
    p.from p.retries p.retry_if_init p.no_auto_start p.autostop
    p.ignore_version p.quiet (Option.value ~default:"None" p.temp_dir)
    (Option.value_map p.timeout ~default:"None" ~f:string_of_int)
    (Option.value_map p.lazy_mode ~default:"None" ~f:Options.lazy_mode_to_string)

  let string_of_open_file {o_open_doc; o_ast; o_live_diagnostics} : string =
    Printf.sprintf "(uri=%s version=%d text=[%d bytes] ast=[%s] diagnostics=[%s])"
      o_open_doc.TextDocumentItem.uri
      o_open_doc.TextDocumentItem.version
      (String.length o_open_doc.TextDocumentItem.text)
      (Option.value_map o_ast ~default:"absent" ~f:(fun _ -> "present"))
      (Option.value_map o_live_diagnostics ~default:"absent"
        ~f:(fun d -> List.length d |> string_of_int))

  let string_of_open_files (files: open_file_info SMap.t) : string =
    SMap.bindings files
      |> List.map ~f:(fun (_,ofi) -> string_of_open_file ofi)
      |> String.concat ","

  let string_of_show_status (show_status: show_status_t) : string =
    match show_status with
    | Never_shown -> "Never_shown"
    | Shown (id_opt, params) -> Printf.sprintf "Shown id=%s params=%s"
        (Option.value_map id_opt ~default:"None" ~f:Lsp_fmt.id_to_string)
        (print_showStatus params |> Hh_json.json_to_string)

  let add_ienv (b: Buffer.t) (ienv: initialized_env) : unit =
    addline b "i_connect_params=" (ienv.i_connect_params |> string_of_connect_params);
    addline b "i_root=" (ienv.i_root |> Path.to_string);
    addline b "i_version=" (ienv.i_version |> Option.value ~default:"None");
    addline b "i_server_id=" (ienv.i_server_id |> string_of_int);
    addline b "i_can_autostart_after_version_mismatch=" (ienv.i_can_autostart_after_version_mismatch
      |> string_of_bool);
    addline b "i_outstanding_local_handlers=" (ienv.i_outstanding_local_handlers
      |> IdMap.bindings |> List.map ~f:(fun (id,_handler) -> Lsp_fmt.id_to_string id)
      |> String.concat ",");
    addline b "i_outstanding_local_requests=" (ienv.i_outstanding_local_requests
      |> IdMap.bindings |> List.map ~f:(fun (id,req) -> Printf.sprintf "%s:%s"
          (Lsp_fmt.id_to_string id) (Lsp_fmt.request_name_to_string req))
      |> String.concat ",");
    addline b "i_outstanding_requests_from_server=" (ienv.i_outstanding_requests_from_server
      |> WrappedMap.bindings
      |> List.map ~f:(fun (id,req) -> Printf.sprintf "#%d:%s:%s"
          id.server_id (Lsp_fmt.id_to_string id.message_id) (Lsp_fmt.request_name_to_string req))
      |> String.concat ",");
    addline b "i_isConnected=" (ienv.i_isConnected |> string_of_bool);
    addline b "i_status=" (ienv.i_status |> string_of_show_status);
    addline b "i_open_files=" (ienv.i_open_files |> string_of_open_files);
    addline b "i_outstanding_diagnostics=" (ienv.i_outstanding_diagnostics
      |> SSet.elements |> String.concat ", ");
    ()

  let add_denv (b: Buffer.t) (denv: disconnected_env) : unit =
    let server_status, watcher_status = match denv.d_server_status with
      | None -> None, None
      | Some (s, w) -> Some s, Some w in
    add_ienv b denv.d_ienv;
    addline b "d_autostart=" (denv.d_autostart |> string_of_bool);
    addline b "d_server_status:server=" (server_status
      |> Option.value_map  ~default:"None" ~f:ServerStatus.string_of_status);
    addline b "d_server_status:watcher=" (watcher_status
      |> Option.value_map  ~default:"None" ~f:FileWatcherStatus.string_of_status);
    ()

  let add_cenv (b: Buffer.t) (cenv: connected_env) : unit =
    let server_status, watcher_status = cenv.c_server_status in
    add_ienv b cenv.c_ienv;
    addline b "c_server_status:server=" (server_status |> ServerStatus.string_of_status);
    addline b "c_server_status:watcher=" (watcher_status
      |> Option.value_map  ~default:"None" ~f:FileWatcherStatus.string_of_status);
    addline b "c_about_to_exit_code=" (cenv.c_about_to_exit_code
      |> Option.value_map ~default:"None" ~f:FlowExitStatus.to_string);
    addline b "c_is_rechecking=" (cenv.c_is_rechecking |> string_of_bool);
    addline b "c_diagnostics=" (cenv.c_diagnostics
      |> SMap.bindings |> List.map ~f:(fun (uri, d) -> Printf.sprintf "%s:%d" uri (List.length d))
      |> String.concat ", ");
    addline b "c_lazy_stats=" (cenv.c_lazy_stats
      |> Option.value_map ~default:"None" ~f:string_of_lazy_stats);
    addline b "c_outstanding_requests_to_server=" (cenv.c_outstanding_requests_to_server
      |> IdSet.elements |> List.map ~f:Lsp_fmt.id_to_string |> String.concat ",");
    addline b "c_outstanding_diagnostics=" (cenv.c_outstanding_diagnostics
      |> SSet.elements |> String.concat ", ");
    ()

  let string_of_state (state: state) : string =
    let b = Buffer.create 10000 in
    begin match state with
      | Pre_init p -> Buffer.add_string b (Printf.sprintf "Pre_init:\n%s\n"
        (string_of_connect_params p))
      | Post_shutdown -> Buffer.add_string b "Post_shutdown:\n[]\n"
      | Disconnected denv -> Buffer.add_string b "Disconnected:\n"; add_denv b denv;
      | Connected cenv -> Buffer.add_string b "Connected:\n"; add_cenv b cenv;
    end;
    Buffer.contents b
end

let do_rage flowconfig_name (state: state) : Rage.result =
  let open Rage in

  (* Some helpers to add various types of data to the rage output... *)
  let add_file (items: rageItem list) (file: Path.t) : rageItem list =
    if Path.file_exists file then
      let data = Path.cat file in (* cat even up to 1gig is workable even if ugly *)
      let len = String.length data in
      let max_len = 10 * 1024 * 1024 in (* maximum 10mb *)
      let data = if len <= max_len then data else String.sub data (len - max_len) max_len in
      { title = Some (Path.to_string file); data; } :: items
    else
      items in
  let add_string (items: rageItem list) (data: string) : rageItem list =
    { title = None; data; } :: items in
  let add_pid (items: rageItem list) ((pid, reason): (int * string)) : rageItem list =
    if String_utils.string_starts_with reason "slave" then
     items
    else
      let pid = string_of_int pid in
      (* some systems have "pstack", some have "gstack", some have neither... *)
      let stack = try Sys_utils.exec_read_lines ~reverse:true ("pstack " ^ pid)
      with _ -> begin
        try Sys_utils.exec_read_lines ~reverse:true ("gstack " ^ pid)
        with e -> ["unable to pstack - " ^ (Printexc.to_string e)]
      end in
      let stack = String.concat "\n" stack in
      add_string items (Printf.sprintf "PSTACK %s (%s) - %s\n\n" pid reason stack)
  in

  let items: rageItem list = [] in

  (* LOGFILES. *)
  (* Where are the logs? Specified explicitly by the user with --log-file and *)
  (* --monitor-log-file when they launched the server. Failing that, the      *)
  (* values in environment variables FLOW_LOG_FILE and FLOW_MONITOR_LOG_FILE  *)
  (* upon launch. Failing that, CommandUtils.server_log_file will look in the *)
  (* flowconfig for a "log.file" option. Failing that it will synthesize one  *)
  (* from `Server_files_js.file_of_root "log"` in the tmp-directory. And      *)
  (* CommandUtils.monitor_log_file is similar except it bypasses flowconfig.  *)
  (* As for tmp dir, that's --temp_dir, failing that FLOW_TEMP_DIR, failing   *)
  (* that temp_dir in flowconfig, failing that Sys_utils.temp_dir_name /flow. *)
  (* WOW! *)
  (* Notionally the only authoritative way to find logs is to connect to a    *)
  (* running monitor and ask it. But we're a 'rage' command whose whole point *)
  (* is to give good answers even when things are not working, e.g. when the  *)
  (* monitor is down. And in any case, by design, a flow client can only ever *)
  (* interact with a server if the client was launched with the same flags    *)
  (* (minimum tmp_dir and flowconfig) as the server was launched with.        *)
  (* Therefore there's no need to ask the monitor. We'll just work with what  *)
  (* log files we'd write to were we ourselves asked to start a server.       *)
  let ienv = match state with
    | Pre_init _ -> None
    | Disconnected denv -> Some denv.d_ienv
    | Connected cenv -> Some cenv.c_ienv
    | Post_shutdown -> None in
  let items = match ienv with
    | None -> items
    | Some ienv ->
      let start_env = CommandUtils.make_env flowconfig_name ienv.i_connect_params ienv.i_root in
      let tmp_dir = start_env.CommandConnect.tmp_dir in
      let server_log_file = Path.make start_env.CommandConnect.log_file in
      (* monitor log file isn't retained anywhere. But since flow lsp doesn't *)
      (* take a --monitor-log-file option, then we know where it must be.     *)
      let monitor_log_file =
         CommandUtils.monitor_log_file flowconfig_name tmp_dir start_env.CommandConnect.root in
      let items = add_file items server_log_file in
      let items = add_file items monitor_log_file in
      (* Let's pick up the old files in case user reported bug after a crash *)
      let items = add_file items (Path.concat server_log_file ".old") in
      let items = add_file items (Path.concat monitor_log_file ".old") in
      (* And the pids file *)
      let items = try
        let pids = PidLog.get_pids (Server_files_js.pids_file ~flowconfig_name ~tmp_dir ienv.i_root)
        in
        Core_list.fold pids ~init:items ~f:add_pid
      with e ->
        let message = Printexc.to_string e in
        let stack = Printexc.get_backtrace () in
        add_string items (Printf.sprintf "Failed to get PIDs: %s - %s" message stack)
      in
      items
  in

  (* CLIENT. This includes the client's perception of the server state. *)
  let items = add_string items ("LSP adapter state: " ^ (RagePrint.string_of_state state) ^ "\n") in

  (* DONE! *)
  items


let parse_json (state: state) (json: Jsonrpc.message) : lsp_message =
  (* to know how to parse a response, we must provide the corresponding request *)
  let outstanding (id: lsp_id) : lsp_request =
    let ienv = match state with
      | Connected env -> env.c_ienv
      | Disconnected env -> env.d_ienv
      | _ -> failwith "Didn't expect an LSP response yet" in
    try
      IdMap.find id ienv.i_outstanding_local_requests
    with Not_found ->
      WrappedMap.find (decode_wrapped id) ienv.i_outstanding_requests_from_server
  in
  Lsp_fmt.parse_lsp json.Jsonrpc.json outstanding

let with_timer (f: unit -> 'a) : (float * 'a) =
  let start = Unix.gettimeofday () in
  let ret = f () in
  let duration = Unix.gettimeofday () -. start in
  duration, ret


(************************************************************************)
(** Main loop                                                          **)
(************************************************************************)

type log_needed = LogNeeded of Persistent_connection_prot.metadata | LogDeferred | LogNotNeeded

let rec main
    base_flags
    (temp_dir: string option)
    (shm_flags: CommandUtils.shared_mem_params)
    (lazy_mode: Options.lazy_mode option)
    (autostop: bool)
    (from: string option)
    ((): unit)
  : unit =
  let connect_params = {
    from = Option.value from ~default:"";
    retries = 0;
    retry_if_init = false;
    timeout = None;
    no_auto_start = false;
    temp_dir;
    autostop;
    lazy_mode;
    shm_flags;
    ignore_version = false;
    quiet = false;
  } in
  let client = Jsonrpc.make_queue () in
  let state = (Pre_init connect_params) in
  main_loop base_flags.Base_flags.flowconfig_name client state

and main_loop flowconfig_name (client: Jsonrpc.queue) (state: state) : unit =
  let event = try Ok (get_next_event state client (parse_json state))
    with e -> Error (state, e, Utils.Callstack (Printexc.get_backtrace ())) in
  let result = match event with
    | Error (state, e, stack) -> Error (state, e, stack, None)
    | Ok event ->
      let (client_duration, result) = with_timer (fun () ->
        try main_handle_unsafe flowconfig_name state event
          with e -> Error (state, e, Utils.Callstack (Printexc.get_backtrace ()))) in
      match result with
      | Ok (state, logneeded) -> Ok (state, logneeded, client_duration)
      | Error (state, e, stack) -> Error (state, e, stack, Some event)
  in
  let state = match result with
    | Ok (state, LogNeeded metadata, client_duration) ->
      let open Persistent_connection_prot in
      let client_duration = if metadata.client_duration = None then Some client_duration
        else metadata.client_duration in
      let metadata = {metadata with client_duration} in
      main_log_command state metadata;
      state
    | Ok (state, _, _) ->
      state
    | Error (state, e, stack, event) -> main_handle_error e stack state event
  in
  main_loop flowconfig_name client state


and main_handle_unsafe flowconfig_name (state: state) (event: event)
  : (state * log_needed, state * exn * Utils.callstack) result =
begin
  match state, event with
  | Pre_init i_connect_params,
    Client_message (RequestMessage (id, InitializeRequest i_initialize_params), metadata) ->
    let i_root = Lsp_helpers.get_root i_initialize_params |> Path.make in
    let d_ienv = {
      i_initialize_params;
      i_connect_params;
      i_root;
      i_version = get_current_version flowconfig_name i_root;
      i_can_autostart_after_version_mismatch = true;
      i_server_id = 0;
      i_outstanding_local_requests = IdMap.empty;
      i_outstanding_local_handlers = IdMap.empty;
      i_outstanding_requests_from_server = WrappedMap.empty;
      i_isConnected = false;
      i_status = Never_shown;
      i_open_files = SMap.empty;
      i_outstanding_diagnostics = SSet.empty;
    } in
    (* If the version in .flowconfig is simply incompatible with our current *)
    (* binary then it doesn't even make sense for us to start up. And future *)
    (* attempts by the client to launch us will fail as well. Clients which  *)
    (* receive the following response are expected to shut down their LSP.   *)
    let required_version = get_current_version flowconfig_name i_root in
    begin match CommandUtils.check_version required_version with
      | Ok () -> ()
      | Error msg -> raise (Error.ServerErrorStart (msg, {Initialize.retry=false;}))
    end;
    let response = ResponseMessage (id, InitializeResult (do_initialize ())) in
    let json = Lsp_fmt.print_lsp response in
    to_stdout json;
    let env = {
      d_ienv;
      d_autostart = true;
      d_server_status = None;
    } in
    Ok (try_connect flowconfig_name env, LogNeeded metadata)

  | _, Client_message (RequestMessage (id, ShutdownRequest), _metadata) ->
    begin match state with Connected env -> close_conn env | _ -> () end;
    let response = ResponseMessage (id, ShutdownResult) in
    let json = Lsp_fmt.print_lsp response in
    to_stdout json;
    Ok (Post_shutdown, LogNotNeeded)

  | _, Client_message (NotificationMessage ExitNotification, _metadata) ->
    if state = Post_shutdown then lsp_exit_ok () else lsp_exit_bad ()

  | _, Client_message (NotificationMessage (CancelRequestNotification _), _metadata) ->
    Ok (state, LogNotNeeded)

  | Pre_init _, Client_message _ ->
    raise (Error.ServerNotInitialized "Server not initialized")

  | _, Client_message ((ResponseMessage (id, result)) as c, metadata) ->
    let ienv = match state with
      | Connected env -> env.c_ienv
      | Disconnected env -> env.d_ienv
      | _ -> failwith "Didn't expect an LSP response yet" in
    begin try
      (* was it a response to a request issued by lspCommand? *)
      let (handle, handle_error) = IdMap.find id ienv.i_outstanding_local_handlers in
      let i_outstanding_local_handlers = IdMap.remove id ienv.i_outstanding_local_handlers in
      let i_outstanding_local_requests = IdMap.remove id ienv.i_outstanding_local_requests in
      let ienv = { ienv with i_outstanding_local_handlers; i_outstanding_local_requests; } in
      let state = match state with
        | Connected env -> Connected { env with c_ienv = ienv; }
        | Disconnected env -> Disconnected { env with d_ienv = ienv; }
        | _ -> failwith "Didn't expect an LSP response to be found yet"
      in
      match result, handle with
      | ShowMessageRequestResult result, ShowMessageHandler handle ->
        Ok (handle result state, LogNotNeeded)
      | ShowStatusResult result, ShowStatusHandler handle -> Ok (handle result state, LogNotNeeded)
      | ErrorResult (e, msg), _ -> Ok (handle_error (e, msg) state, LogNotNeeded)
      | _ -> failwith (Printf.sprintf "Response %s has mistyped handler" (message_name_to_string c))
    with Not_found ->
      (* if not, it must be a response to a request issued by the server *)
      match state with
      | Connected cenv ->
        let (state, _) = track_to_server state c in
        let wrapped = decode_wrapped id in (* only forward responses if they're to current server *)
        if wrapped.server_id = cenv.c_ienv.i_server_id then send_lsp_to_server cenv metadata c;
        Ok (state, LogNotNeeded)
      | _ ->
        failwith (Printf.sprintf "Response %s has missing handler" (message_name_to_string c))
    end

  | _, Client_message (RequestMessage (id, DocumentSymbolRequest params), metadata) ->
    (* documentSymbols is handled in the client, not the server, since it's *)
    (* purely syntax-driven and we'd like it to work even if the server is  *)
    (* busy or disconnected *)
    let state = do_documentSymbol flowconfig_name state id params in
    Ok (state, LogNeeded metadata)

  | Connected cenv, Client_message (c, metadata) ->
    (* We'll track what's being sent to the server. This might involve some client *)
    (* computation work, which we'll profile, and send it over in metadata. *)
    let client_duration, state = with_timer (fun () ->
      let state, {changed_live_uri} = track_to_server state c in
      let state = Option.value_map changed_live_uri ~default:state
        ~f:(do_live_diagnostics flowconfig_name state) in
      state) in
    let metadata = { metadata with
      Persistent_connection_prot.client_duration=Some client_duration } in
    send_lsp_to_server cenv metadata c;
    Ok (state, LogDeferred)

  | _, Client_message (RequestMessage (id, RageRequest), metadata) ->
    (* How to handle a rage request? If we're connected to a server, then the *)
    (* above case will just have forwarded the message on to the server (and  *)
    (* we'll patch in our own extra information when the server replies). But *)
    (* if there's no server then we have to reply here and now.               *)
    let result = do_rage flowconfig_name state in
    let response = ResponseMessage (id, RageResult result) in
    let json = Lsp_fmt.print_lsp response in
    to_stdout json;
    Ok (state, LogNeeded metadata)

  | _, Client_message ((NotificationMessage (DidOpenNotification _)) as c, metadata)
  | _, Client_message ((NotificationMessage (DidChangeNotification _)) as c, metadata)
  | _, Client_message ((NotificationMessage (DidSaveNotification _)) as c, metadata)
  | _, Client_message ((NotificationMessage (DidCloseNotification _)) as c, metadata) ->
    (* these are editor events that happen while disconnected. *)
    let client_duration, state = with_timer (fun () ->
      let state, {changed_live_uri} = track_to_server state c in
      let state = Option.value_map changed_live_uri ~default:state
        ~f:(do_live_diagnostics flowconfig_name state) in
      state) in
    let metadata = { metadata with
      Persistent_connection_prot.client_duration=Some client_duration } in
    Ok (state, LogNeeded metadata)

  | Disconnected _, Client_message (c, _metadata) ->
    let (state, _) = track_to_server state c in
    let method_ = Lsp_fmt.denorm_message_to_string c in
    let e = Error.RequestCancelled ("Server not connected; can't handle " ^ method_) in
    let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
    Error (state, e, Utils.Callstack stack)

  | Post_shutdown, Client_message (_, _metadata) ->
    raise (Error.RequestCancelled "Server shutting down")

  | Connected cenv, Server_message (Persistent_connection_prot.ServerExit exit_code) ->
    let state = Connected { cenv with c_about_to_exit_code = Some exit_code; } in
    Ok (state, LogNotNeeded)

  | Connected cenv, Server_message (Persistent_connection_prot.LspFromServer (msg, metadata)) ->
    let state, metadata = match msg with
      | None -> state, metadata
      | Some outgoing ->
        let state = track_from_server state outgoing in
        let outgoing, metadata = match outgoing with
          | RequestMessage (id, request) ->
            let wrapped = { server_id = cenv.c_ienv.i_server_id; message_id = id; } in
            RequestMessage (encode_wrapped wrapped, request), metadata
          | ResponseMessage (id, RageResult items) ->
            (* we'll zero out the "client_duration", which at the moment represents client-side *)
            (* work we did before sending out the request. By zeroing it out now, it'll get *)
            (* filled out with the client-side work that gets done right here and now. *)
            let metadata = { metadata with Persistent_connection_prot.client_duration = None} in
            ResponseMessage (id, RageResult (items @ (do_rage flowconfig_name state))), metadata
          | _ -> outgoing, metadata
        in
        to_stdout (Lsp_fmt.print_lsp outgoing);
        state, metadata
    in
    Ok (state, LogNeeded metadata)

  | Connected cenv, Server_message (Persistent_connection_prot.Errors {errors; warnings}) ->
    (* A note about the errors reported by this server message:               *)
    (* While a recheck is in progress, between StartRecheck and EndRecheck,   *)
    (* the server will periodically send errors+warnings. These are additive  *)
    (* to the errors which have previously been reported. Once the recheck    *)
    (* has finished then the server will send a new exhaustive set of errors. *)
    (* At this opportunity we should erase all errors not in this set.        *)
    (* This differs considerably from the semantics of LSP publishDiagnostics *)
    (* which says "whenever you send publishDiagnostics for a file, that      *)
    (* now contains the complete truth for that file."                        *)

    (* I hope that flow won't produce errors with an empty path. But such errors are *)
    (* fatal to Nuclide, so if it does, then we'll at least use a fall-back path.    *)
    let default_uri = cenv.c_ienv.i_root |> Path.to_string |> File_url.create in
    (* 'all' is an SMap from uri to diagnostic list, and 'add' appends the error within the map *)
    let add severity error all =
      let uri, diagnostic = error_to_lsp ~severity ~default_uri error in
      SMap.add ~combine:List.append uri [diagnostic] all in
    (* First construct an SMap from uri to diagnostic list, which gathers together *)
    (* all the errors and warnings per uri *)
    let all = Errors.ErrorSet.fold (add (Some PublishDiagnostics.Error)) errors SMap.empty in
    let all = Errors.ErrorSet.fold (add (Some PublishDiagnostics.Warning)) warnings all
    in
    if cenv.c_is_rechecking then
      Ok (do_additional_diagnostics cenv all, LogNotNeeded)
    else
      Ok (do_replacement_diagnostics cenv all, LogNotNeeded)

  | Connected cenv, Server_message Persistent_connection_prot.StartRecheck ->
    let state = show_recheck_progress { cenv with
      c_is_rechecking = true;
      c_lazy_stats = None;
    } in
    Ok (state, LogNotNeeded)

  | Connected cenv, Server_message Persistent_connection_prot.EndRecheck lazy_stats ->
    let state = show_recheck_progress { cenv with
      c_is_rechecking = false;
      c_lazy_stats = Some lazy_stats;
    } in
    Ok (state, LogNotNeeded)

  | Connected cenv, Server_message (Persistent_connection_prot.Please_hold status) ->
    let (server_status, watcher_status) = status in
    let c_server_status = (server_status, Some watcher_status) in
    (* We keep a log of typecheck summaries over the past 2mins. *)
    let c_recent_summaries = cenv.c_recent_summaries in
    let new_time = Unix.gettimeofday () in
    let summary = ServerStatus.get_summary server_status in
    let c_recent_summaries = Option.value_map summary ~default:c_recent_summaries ~f:(fun summary ->
      (new_time, summary) :: cenv.c_recent_summaries
        |> List.filter ~f:(fun (t,_) -> t >= new_time -. 120.0))
    in
    let state = show_recheck_progress { cenv with c_server_status; c_recent_summaries; } in
    Ok (state, LogNotNeeded)

  | _, Server_message _ ->
    failwith (Printf.sprintf "In state %s, unexpected event %s"
      (string_of_state state) (denorm_string_of_event event))

  | Disconnected env, Tick ->
    let state = try_connect flowconfig_name env in
    Ok (state, LogNotNeeded)

  | _, Tick ->
    Lwt.async EventLoggerLwt.flush;
    Ok (state, LogNotNeeded)
end


and main_log_command
    (state: state)
    (metadata: Persistent_connection_prot.metadata)
  : unit =
  let open Persistent_connection_prot in
  let client_context = FlowEventLogger.get_context () in
  let request = metadata.start_json_truncated |> Hh_json.json_to_string in
  let wall_start = metadata.start_wall_time in
  let server_profiling = metadata.server_profiling in
  let client_duration = metadata.client_duration in
  let extra_data = metadata.extra_data in
  let persistent_context = Some { FlowEventLogger.
    start_lsp_state = metadata.start_lsp_state;
    start_lsp_state_reason = metadata.start_lsp_state_reason;
    start_server_status = Option.map metadata.start_server_status
      ~f:(ServerStatus.string_of_status ~terse:true);
    start_watcher_status = Option.map metadata.start_watcher_status
      ~f:(FileWatcherStatus.string_of_status);
  } in
  let server_logging_context = metadata.server_logging_context in
  (* gather any recent typechecks that finished after the request had arrived *)
  let delays = match state with
    | Connected cenv -> Core_list.filter_map cenv.c_recent_summaries ~f:(fun (t,s) ->
      if t > wall_start then Some s else None)
    | _ -> [] in
  let root = Option.value ~default:Path.dummy_path (get_root state) in
  let persistent_delay = if delays = [] then None
    else Some (ServerStatus.log_of_summaries ~root delays) in

  match metadata.error_info with
  | None -> FlowEventLogger.persistent_command_success
      ~server_logging_context ~request ~extra_data
      ~client_context ~persistent_context ~persistent_delay
      ~server_profiling ~client_duration ~wall_start ~error:None
  | Some (ExpectedError, msg, stack) -> FlowEventLogger.persistent_command_success
      ~server_logging_context ~request ~extra_data
      ~client_context ~persistent_context ~persistent_delay
      ~server_profiling ~client_duration ~wall_start ~error:(Some (msg, stack))
  | Some (UnexpectedError, msg, stack) -> FlowEventLogger.persistent_command_failure
      ~server_logging_context ~request ~extra_data
      ~client_context ~persistent_context ~persistent_delay
      ~server_profiling ~client_duration ~wall_start ~error:(msg, stack)

and main_log_error ~(expected: bool) (msg: string) (stack: string) : unit =
  let error = (msg, Utils.Callstack stack) in
  let client_context = FlowEventLogger.get_context () in
  match expected with
  | true -> FlowEventLogger.persistent_expected_error ~client_context ~error
  | false -> FlowEventLogger.persistent_unexpected_error ~client_context ~error

and main_handle_error
    (e: exn)
    (Utils.Callstack stack)
    (state: state)
    (event: event option)
  : state =
  let open Marshal_tools in
  match e with
  | Server_fatal_connection_exception _edata when state = Post_shutdown ->
    state

  | Server_fatal_connection_exception edata -> begin
    (* log the error *)
    let stack = edata.stack ^ "---\n" ^ stack in
    main_log_error ~expected:true ("[Server fatal] " ^ edata.message) stack;
    (* report that we're disconnected to telemetry/connectionStatus *)
    let state = begin match state with
      | Connected env ->
        let i_isConnected = Lsp_helpers.notify_connectionStatus env.c_ienv.i_initialize_params
          to_stdout env.c_ienv.i_isConnected false in
        let env = { env with c_ienv = { env.c_ienv with i_isConnected; }; } in
        Connected env
      | _ -> state
    end in
    (* send the error report *)
    let code = match state with
      | Connected cenv -> cenv.c_about_to_exit_code
      | _ -> None in
    let code = Option.value_map code ~f:FlowExitStatus.to_string ~default:"" in
    let report = Printf.sprintf "Server fatal exception: [%s] %s\n%s" code edata.message stack in
    Lsp_helpers.telemetry_error to_stdout report;
    let d_autostart, d_ienv = match state with
      | Connected { c_ienv; c_about_to_exit_code; _ }
        when c_about_to_exit_code = Some FlowExitStatus.Flowconfig_changed
          || c_about_to_exit_code = Some FlowExitStatus.Server_out_of_date ->
        (* we allow at most one autostart_after_version_mismatch per *)
        (* instance so as to avoid getting into version battles.     *)
        let previous = c_ienv.i_can_autostart_after_version_mismatch in
        let d_ienv = { c_ienv with i_can_autostart_after_version_mismatch = false; } in
        previous, d_ienv
      | Connected { c_ienv; _ } ->
        false, c_ienv
      | Disconnected { d_ienv; _ } ->
        false, d_ienv
      | Pre_init _
      | Post_shutdown ->
        failwith "Unexpected server error in inapplicable state" (* crash *)
    in
    let env = {
      d_ienv;
      d_autostart;
      d_server_status = None;
    }
    in
    let _state = state |> dismiss_tracks in
    let state = Disconnected env in
    state
    end

  | Client_recoverable_connection_exception edata ->
    let stack = edata.stack ^ "---\n" ^ stack in
    main_log_error ~expected:true ("[Client recoverable] " ^ edata.message) stack;
    let report = Printf.sprintf "Client exception: %s\n%s" edata.message stack in
    Lsp_helpers.telemetry_error to_stdout report;
    state

  | Client_fatal_connection_exception edata ->
    let stack = edata.stack ^ "---\n" ^ stack in
    main_log_error ~expected:true ("[Client fatal] " ^ edata.message) stack;
    let report = Printf.sprintf "Client fatal exception: %s\n%s" edata.message stack in
    Printf.eprintf "%s" report;
    lsp_exit_bad ()

  | e ->
    let (code, message, _data) = get_error_info e in
    main_log_error ~expected:true ("[FlowLSP] " ^ message) stack;
    let text = Printf.sprintf "FlowLSP exception %s [%i]\n%s" message code stack in
    let () = match event with
      | Some (Client_message (RequestMessage (id, _request), _metadata)) ->
        let json = Lsp_fmt.print_lsp_response id (ErrorResult (e, stack)) in
        to_stdout json;
      | _ ->
        Lsp_helpers.telemetry_error to_stdout text
    in
    state


let command = CommandSpec.command spec main
