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

type initialized_env = {
  i_initialize_params: Lsp.Initialize.params;
  i_connect_params: command_params;
  i_root: Path.t;
  i_version: string option;
  i_server_id: int;
  i_can_autostart_after_version_mismatch: bool;
  i_outstanding_local_handlers: state lsp_handler IdMap.t;
  i_outstanding_local_requests: lsp_request IdMap.t;
  i_outstanding_requests_from_server: Lsp.lsp_request WrappedMap.t;
  i_isConnected: bool; (* what we've told the client about our connection status *)
}

and disconnected_env = {
  d_ienv: initialized_env;
  d_editor_open_files: Lsp.TextDocumentItem.t SMap.t;
  d_autostart: bool;
  d_start_time: float;
  d_server_status: (ServerStatus.status * FileWatcherStatus.status) option;
  d_dialog_stopped: ShowMessageRequest.t; (* "Flow server is stopped. [Restart]" *)
  d_dialog_stopped_can_show: bool; (* if the user has already dismissed it, don't reshow *)
  d_actionRequired_stopped: ActionRequired.t; (* "Flow server is stopped." *)
  d_dialog_connecting: ShowMessageRequest.t; (* "Connecting to Flow server." *)
  d_dialog_connecting_can_show: bool; (* if the user has already dismissed it, don't reshow *)
  d_progress_connecting: Progress.t; (* e.g. "Connecting... busy/initializing [53s]" *)
}

and connected_env = {
  c_ienv: initialized_env;
  c_conn: server_conn;
  c_server_status: ServerStatus.status * (FileWatcherStatus.status option);
  c_editor_open_files: Lsp.TextDocumentItem.t SMap.t;
  c_about_to_exit_code: FlowExitStatus.t option;
  c_dialog_connected: ShowMessageRequest.t; (* "Connected to Flow server" *)
  (* stateful handling of Errors messages from server... *)
  c_is_rechecking: bool;
  c_diagnostics: PublishDiagnostics.diagnostic list SMap.t;
  c_recheck_progress: Progress.t; (* e.g. "Rechecking merging 5/20 files" *)
  (* if server gets disconnected, we will tidy up these things... *)
  c_outstanding_requests_to_server: Lsp.IdSet.t;
  c_outstanding_progress: ISet.t; (* we'll send progress(null) *)
  c_outstanding_action: ISet.t; (* we'll send action(null) *)
  c_outstanding_diagnostics: SSet.t; (* we'll send publishDiagnostics([]) *)
}

and state =
  (* Pre_init: we haven't yet received the initialize request.           *)
  | Pre_init of command_params
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
  | Client_message of Lsp.lsp_message
  | Tick (* once per second, on idle *)


let string_of_state (state: state) : string =
  match state with
  | Pre_init _ -> "Pre_init"
  | Disconnected _ -> "Disconnected"
  | Connected env -> "Connected." ^
      (Option.value_map env.c_about_to_exit_code ~default:"" ~f:FlowExitStatus.to_string)
  | Post_shutdown -> "Post_shutdown"


let string_of_event (event: event) : string =
  match event with
  | Server_message response ->
    Printf.sprintf "Server_message(%s)" (Persistent_connection_prot.string_of_response response)
  | Client_message c ->
    Printf.sprintf "Client_message(%s)" (Lsp_fmt.message_to_string c)
  | Tick ->
    "Tick"


let to_stdout (json: Hh_json.json) : unit =
  (* Extra \r\n purely for easier logfile reading; not required by protocol. *)
  let s = (Hh_json.json_to_string json) ^ "\r\n\r\n" in
  Http_lite.write_message stdout s

let get_current_version (root: Path.t) : string option =
  Server_files_js.config_file root
    |> FlowConfig.get ~allow_cache:false
    |> FlowConfig.required_version


let get_editor_open_files (state: state) : Lsp.TextDocumentItem.t SMap.t option =
  match state with
  | Connected cenv -> Some cenv.c_editor_open_files
  | Disconnected denv -> Some denv.d_editor_open_files
  | _ -> None


let get_next_event_from_server (fd: Unix.file_descr) : event =
  try
    Server_message (Marshal_tools.from_fd_with_preamble fd)
  with e ->
    let message = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    raise (Server_fatal_connection_exception { Marshal_tools.message; stack; })

let get_next_event_from_client
    (client: Jsonrpc.queue)
    (parser: Jsonrpc.message -> Lsp.lsp_message)
  : event =
  let message = Jsonrpc.get_message client in
  match message with
  | `Message c -> Client_message (parser c)
  | `Fatal_exception edata -> raise (Client_fatal_connection_exception edata)
  | `Recoverable_exception edata -> raise (Client_recoverable_connection_exception edata)

let get_next_event
    (state: state)
    (client: Jsonrpc.queue)
    (parser: Jsonrpc.message -> Lsp.lsp_message)
  : event =
  if Jsonrpc.has_message client then
    get_next_event_from_client client parser
  else
    let client_fd = Jsonrpc.get_read_fd client in
    match state with
    | Connected { c_conn; _ } ->
      let server_fd = Timeout.descr_of_in_channel c_conn.ic in
      let fds, _, _ = Unix.select [server_fd; client_fd] [] [] 1.0 in
      if fds = [] then Tick
      else if List.mem fds server_fd then get_next_event_from_server server_fd
      else get_next_event_from_client client parser
    | _ ->
      let fds, _, _ = Unix.select [client_fd] [] [] 1.0 in
      if fds = [] then Tick
      else get_next_event_from_client client parser


let showMessageRequest
    (handler: state lsp_handler)
    (type_: MessageType.t)
    (message: string)
    (titles: string list)
    (ienv: initialized_env)
  : (ShowMessageRequest.t * initialized_env) =
  let id = NumberId (Jsonrpc.get_next_request_id ()) in
  let actions = List.map titles ~f:(fun title -> { ShowMessageRequest.title; }) in
  let request = ShowMessageRequestRequest { ShowMessageRequest.type_; message;  actions; } in
  let json = Lsp_fmt.print_lsp (RequestMessage (id, request)) in
  to_stdout json;
  let i_outstanding_local_requests = IdMap.add id request ienv.i_outstanding_local_requests in
  let i_outstanding_local_handlers = IdMap.add id handler ienv.i_outstanding_local_handlers in
  let ienv = { ienv with i_outstanding_local_requests; i_outstanding_local_handlers; } in
  (ShowMessageRequest.Present { id; }, ienv)

(* This function merely posts a $/cancelRequest notification; the client   *)
(* will respond asynchronously, maybe with RequestCancelled response, or   *)
(* maybe with a real respoonse.                                            *)
let dismiss_showMessageRequest (dialog: ShowMessageRequest.t) : ShowMessageRequest.t =
  begin match dialog with
    | ShowMessageRequest.Absent -> ()
    | ShowMessageRequest.Present { id; _ } ->
      let notification = CancelRequestNotification { CancelRequest.id; } in
      let json = Lsp_fmt.print_lsp (NotificationMessage notification) in
      to_stdout json
  end;
  ShowMessageRequest.Absent


let send_to_server (env: connected_env) (request: Persistent_connection_prot.request) : unit =
  let _bytesWritten =
    Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel env.c_conn.oc) request in
  ()

let send_lsp_to_server (env: connected_env) (message: lsp_message) : unit =
  send_to_server env (Persistent_connection_prot.LspToServer message)


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
      referencesProvider = false;
      documentHighlightProvider = false;
      documentSymbolProvider = false;
      workspaceSymbolProvider = false;
      codeActionProvider = false;
      codeLensProvider = None;
      documentFormattingProvider = false;
      documentRangeFormattingProvider = false;
      documentOnTypeFormattingProvider = Some {
        firstTriggerCharacter = ";";
        moreTriggerCharacter = ["}"];
      };
      renameProvider = false;
      documentLinkProvider = None;
      executeCommandProvider = None;
      typeCoverageProvider = false;
      rageProvider = false;
    }
  }


let dismiss_ui (state: state) : state =
  match state with
  | Pre_init _ -> state
  | Post_shutdown -> state
  | Disconnected env ->
    let d_dialog_stopped = dismiss_showMessageRequest env.d_dialog_stopped in
    let d_actionRequired_stopped = Lsp_helpers.notify_actionRequired env.d_ienv.i_initialize_params
      to_stdout env.d_actionRequired_stopped None in
    let d_dialog_connecting = dismiss_showMessageRequest env.d_dialog_connecting in
    let d_progress_connecting = Lsp_helpers.notify_progress env.d_ienv.i_initialize_params
      to_stdout env.d_progress_connecting None in
    Disconnected { env with
      d_dialog_stopped; d_actionRequired_stopped; d_dialog_connecting; d_progress_connecting;
    }
  | Connected env ->
    let c_dialog_connected = dismiss_showMessageRequest env.c_dialog_connected in
    Connected { env with c_dialog_connected; }


let show_connected (start_time: float) (env: connected_env) : state =
  (* report that we're connected to telemetry/connectionStatus *)
  let i_isConnected = Lsp_helpers.notify_connectionStatus env.c_ienv.i_initialize_params
    to_stdout env.c_ienv.i_isConnected true in
  let env = { env with c_ienv = { env.c_ienv with i_isConnected; }; } in
  (* show a notification if necessary *)
  let time = Unix.gettimeofday () in
  if (time -. start_time <= 30.0) then
    Connected env
  else
    let seconds = int_of_float (time -. start_time) in
    let msg = Printf.sprintf "Flow server is now ready, after %i seconds." seconds in
    let clear_flag state = match state with
      | Connected cenv ->
        Connected { cenv with c_dialog_connected = ShowMessageRequest.Absent; }
      | _ -> state in
    let handle_error (_e, _stack) state = clear_flag state in
    let handle_result _r state = clear_flag state in
    let handle = (ShowMessageHandler handle_result, handle_error) in
    let (c_dialog_connected, c_ienv) = showMessageRequest handle
      MessageType.InfoMessage msg [] env.c_ienv in
    Connected { env with c_ienv; c_dialog_connected; }


let show_connecting (reason: CommandConnectSimple.error) (env: disconnected_env) : state =
  let d_dialog_stopped = dismiss_showMessageRequest env.d_dialog_stopped in
  let d_actionRequired_stopped = Lsp_helpers.notify_actionRequired env.d_ienv.i_initialize_params
    to_stdout env.d_actionRequired_stopped None in

  if reason = CommandConnectSimple.Server_missing then
    Lsp_helpers.log_info to_stdout "Starting Flow server";

  let (d_dialog_connecting, d_ienv) = match env.d_dialog_connecting with
    | ShowMessageRequest.Present _ -> env.d_dialog_connecting, env.d_ienv
    | ShowMessageRequest.Absent when not env.d_dialog_connecting_can_show ->
      env.d_dialog_connecting, env.d_ienv
    | ShowMessageRequest.Absent -> begin
      let clear_flag state = match state with
        | Disconnected e -> Disconnected { e with
            d_dialog_connecting = ShowMessageRequest.Absent;
            d_dialog_connecting_can_show = false;
          }
        | _ -> state in
      let handle_error (_e, _stack) state = clear_flag state in
      let handle_result _r state = clear_flag state in
      let handle = (ShowMessageHandler handle_result, handle_error) in
      showMessageRequest handle MessageType.InfoMessage  "Flow: connecting to server" [] env.d_ienv
    end in

  let msg = match reason, env.d_server_status with
    | CommandConnectSimple.Server_missing, _ -> "Flow: Server starting"
    | CommandConnectSimple.Server_socket_missing, _ -> "Flow: Server starting?"
    | CommandConnectSimple.Build_id_mismatch, _ -> "Flow: Server is wrong version"
    | CommandConnectSimple.Server_busy (CommandConnectSimple.Too_many_clients), _ ->
      "Flow: Server busy"
    | CommandConnectSimple.Server_busy _, None -> "Flow: Server busy"
    | CommandConnectSimple.Server_busy _, Some (server_status, watcher_status) ->
      if not (ServerStatus.is_free server_status) then
        "Flow: " ^ (ServerStatus.string_of_status ~use_emoji:true server_status)
      else
        "Flow: " ^ (FileWatcherStatus.string_of_status watcher_status) in
  let d_progress_connecting = Lsp_helpers.notify_progress env.d_ienv.i_initialize_params
    to_stdout env.d_progress_connecting (Some msg)

  in
  Disconnected { env with
    d_ienv;
    d_dialog_stopped;
    d_actionRequired_stopped;
    d_dialog_connecting;
    d_progress_connecting;
  }


let show_disconnected
    (code: FlowExitStatus.t option)
    (msg: string option)
    (env: disconnected_env)
  : state =
  (* report that we're disconnected to telemetry/connectionStatus *)
  let i_isConnected = Lsp_helpers.notify_connectionStatus env.d_ienv.i_initialize_params
    to_stdout env.d_ienv.i_isConnected false in
  let env = { env with d_ienv = { env.d_ienv with i_isConnected; }; } in

  (* update dialogs/progress as necessary *)
  let d_dialog_connecting = dismiss_showMessageRequest env.d_dialog_connecting in
  let d_progress_connecting = Lsp_helpers.notify_progress env.d_ienv.i_initialize_params to_stdout
    env.d_progress_connecting None in

  let msg = Option.value msg ~default:"Flow: server is stopped" in
  let full_msg = match code with
    | Some code -> Printf.sprintf "%s [%s]" msg (FlowExitStatus.to_string code)
    | None -> msg
  in

  let (d_dialog_stopped, d_ienv) = match env.d_dialog_stopped with
    | ShowMessageRequest.Present _ -> env.d_dialog_stopped, env.d_ienv
    | ShowMessageRequest.Absent when not env.d_dialog_stopped_can_show ->
      env.d_dialog_stopped, env.d_ienv
    | ShowMessageRequest.Absent -> begin
      let handle_error (_e, _stack) state = match state with
        | Disconnected e -> Disconnected { e with d_dialog_stopped = ShowMessageRequest.Absent; }
        | _ -> state in
      let handle_result result state = match state, result with
        | Disconnected e, Some { ShowMessageRequest.title = "Restart"; } ->
          (* on the next tick, try_connect will invoke start_flow_server... *)
          Disconnected { e with
            d_dialog_stopped = ShowMessageRequest.Absent;
            d_autostart = true;
          }
        | Disconnected e, _ ->
          (* on subsequent ticks, try_connect won't display errors and won't autostart... *)
          Disconnected { e with
            d_dialog_stopped = ShowMessageRequest.Absent;
            d_dialog_stopped_can_show = false;
          }
        | _ ->
          state in
      let handle = (ShowMessageHandler handle_result, handle_error) in
      showMessageRequest handle MessageType.ErrorMessage full_msg ["Restart"] env.d_ienv
      end in

  let d_actionRequired_stopped = Lsp_helpers.notify_actionRequired env.d_ienv.i_initialize_params
    to_stdout env.d_actionRequired_stopped (Some full_msg)

  in
  Disconnected { env with
    d_ienv;
    d_dialog_stopped;
    d_actionRequired_stopped;
    d_dialog_connecting;
    d_progress_connecting;
  }


let try_connect (env: disconnected_env) : state =
  (* If the version in .flowconfig has changed under our feet then we mustn't *)
  (* connect. We'll terminate and trust the editor to relaunch an ok version. *)
  let current_version = get_current_version env.d_ienv.i_root in
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
  let start_env = CommandUtils.make_env env.d_ienv.i_connect_params env.d_ienv.i_root in

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
    ~client_handshake ~tmp_dir:start_env.CommandConnect.tmp_dir start_env.CommandConnect.root in

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
      c_dialog_connected = ShowMessageRequest.Absent;
      c_is_rechecking = false;
      c_diagnostics = SMap.empty;
      c_recheck_progress = Progress.Absent;
      c_outstanding_requests_to_server = Lsp.IdSet.empty;
      c_outstanding_progress = ISet.empty;
      c_outstanding_action = ISet.empty;
      c_outstanding_diagnostics = SSet.empty;
      c_editor_open_files = env.d_editor_open_files;
    } in
    (* send the initial messages to the server *)
    send_to_server new_env Persistent_connection_prot.Subscribe;
    let make_open_message (textDocument: TextDocumentItem.t) : lsp_message =
      NotificationMessage (DidOpenNotification { DidOpen.textDocument; }) in
    let open_messages = env.d_editor_open_files |> SMap.bindings |> List.map ~f:snd
      |> List.map ~f:make_open_message in
    List.iter open_messages ~f:(send_lsp_to_server new_env);
    (* close the old UI and bring up the new *)
    let _state = dismiss_ui (Disconnected env) in
    let new_state = show_connected env.d_start_time new_env in
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
      CommandMeanKill.mean_kill ~tmp_dir root;
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


let dismiss_connected_dialog_if_necessary (state: state) (event: event) : state =
  match state, event with
  | Connected env, Client_message (Lsp.RequestMessage _)
  | Connected env, Client_message (Lsp.NotificationMessage _) ->
    let c_dialog_connected = dismiss_showMessageRequest env.c_dialog_connected in
    Connected { env with c_dialog_connected; }
  | _ -> state


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

let track_to_server (state: state) (c: Lsp.lsp_message) : state =
  (* didOpen, didChange, didSave, didClose: save them up in editor_file_events *)
  let editor_open_files = match (get_editor_open_files state), c with
    | Some editor_open_files, NotificationMessage (DidOpenNotification params) ->
      let doc = params.DidOpen.textDocument in
      let uri = params.DidOpen.textDocument.TextDocumentItem.uri in
      SMap.add uri doc editor_open_files

    | Some editor_open_files, NotificationMessage (DidCloseNotification params) ->
      let uri = params.DidClose.textDocument.TextDocumentIdentifier.uri in
      SMap.remove uri editor_open_files

    | Some editor_open_files, NotificationMessage (DidChangeNotification params) ->
      let uri = params.DidChange.textDocument.VersionedTextDocumentIdentifier.uri in
      let doc = SMap.find uri editor_open_files in
      let text = doc.TextDocumentItem.text in
      let doc' = { Lsp.TextDocumentItem.
        uri;
        languageId = doc.TextDocumentItem.languageId;
        version = params.DidChange.textDocument.VersionedTextDocumentIdentifier.version;
        text = Lsp_helpers.apply_changes_unsafe text params.DidChange.contentChanges;
      } in
      SMap.add uri doc' editor_open_files

    | Some editor_open_files, _ ->
      editor_open_files

    | None, _ ->
      SMap.empty
  in
  let state = match state with
    | Connected env -> Connected { env with c_editor_open_files = editor_open_files; }
    | Disconnected env -> Disconnected { env with d_editor_open_files = editor_open_files; }
    | _ -> state
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
  state


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
  (* server->client publishDiagnostics *)
  | Connected env, NotificationMessage (PublishDiagnosticsNotification params) ->
    (* publishDiagnostics: save up all URIs with non-empty diagnostics *)
    let uri = params.PublishDiagnostics.uri in
    let published = params.PublishDiagnostics.diagnostics in
    let c_outstanding_diagnostics = match published with
      | [] -> SSet.remove uri env.c_outstanding_diagnostics
      | _ -> SSet.add uri env.c_outstanding_diagnostics in
    Connected { env with c_outstanding_diagnostics }
  (* server->client progress *)
  | Connected env, NotificationMessage (ProgressNotification params) ->
    let id = params.Progress.id in
    let label = params.Progress.label in
    let c_outstanding_progress = match label with
      | None -> ISet.remove id env.c_outstanding_progress
      | Some _ -> ISet.add id env.c_outstanding_progress in
    Connected { env with c_outstanding_progress }
  (* server->client actionRequired *)
  | Connected env, NotificationMessage (ActionRequiredNotification params) ->
    let id = params.ActionRequired.id in
    let label = params.ActionRequired.label in
    let c_outstanding_action = match label with
      | None -> ISet.remove id env.c_outstanding_action
      | Some _ -> ISet.add id env.c_outstanding_action in
    Connected { env with c_outstanding_action }
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
  let close_progress (id: int) : unit =
    let notification = ProgressNotification { Progress.id; label = None; } in
    let json = Lsp_fmt.print_lsp_notification notification in
    to_stdout json
  in
  let close_actionRequired (id: int) : unit =
    let notification = ActionRequiredNotification { ActionRequired.id; label = None; } in
    let json = Lsp_fmt.print_lsp_notification notification in
    to_stdout json
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
    ISet.iter close_progress env.c_outstanding_progress;
    ISet.iter close_actionRequired env.c_outstanding_action;
    SSet.iter clear_diagnostics env.c_outstanding_diagnostics;
    Connected { env with
      c_outstanding_requests_to_server = IdSet.empty;
      c_outstanding_progress = ISet.empty;
      c_outstanding_action = ISet.empty;
      c_outstanding_diagnostics = SSet.empty;
    }
  | _ -> state


(******************************************************************************)
(* Diagnostics                                                                *)
(* These should really be handle inside the flow server so it sends out       *)
(* LSP publishDiagnostics notifications and we track them in the normal way.  *)
(* But while the flow server has to handle legacy clients as well as LSP      *)
(* clients, we don't want to make the flow server code too complex, so we're  *)
(* handling them here for now.                                                *)
(******************************************************************************)

(* print_diagnostics: just pushes the set of diagnostis for this uri to the client *)
let print_diagnostics
    (uri: string)
    (diagnostics: PublishDiagnostics.diagnostic list)
    (state: state)
  : state =
  let msg = NotificationMessage
    (PublishDiagnosticsNotification { PublishDiagnostics.uri; diagnostics; }) in
  let state = track_from_server state msg in
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
  let state = SSet.fold (fun uri state -> print_diagnostics uri [] state) now_empty_uris state in

  (* Send publishDiagnostics for all files that have diagnostics *)
  let state = SMap.fold print_diagnostics c_diagnostics state
  in
  state


let show_recheck_progress (cenv: connected_env) : state =
  (* TODO: this functionality should be moved inside flow server... *)
  let writer state params =
    let msg = NotificationMessage (ProgressNotification params) in
    let state = track_from_server state msg in
    to_stdout (Lsp_fmt.print_lsp msg);
    state
  in
  let label = match cenv.c_is_rechecking, cenv.c_server_status with
    | true, (server_status, _) when not (ServerStatus.is_free server_status) ->
      Some ("Flow: " ^ (ServerStatus.string_of_status ~use_emoji:true server_status))
    | true, _ -> Some "Flow: Server is rechecking..."
    | false, _ -> None in
  let state = Connected cenv in

  let (state, c_recheck_progress) = Lsp_helpers.notify_progress_raw state
    cenv.c_ienv.i_initialize_params writer cenv.c_recheck_progress label in

  let state = match state with
    | Connected cenv -> Connected { cenv with c_recheck_progress; }
    | _ -> failwith "notify_progress shouldn't be changing state so much" in
  state

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


(************************************************************************)
(** Main loop                                                          **)
(************************************************************************)

let rec main
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
  main_loop client state

and main_loop (client: Jsonrpc.queue) (state: state) : unit =
  let state = main_handle client state in
  main_loop client state

and main_handle (client: Jsonrpc.queue) (state: state) : state =
  try
    let event = get_next_event state client (parse_json state) in
    try
      let state2 = dismiss_connected_dialog_if_necessary state event in
      try
        main_handle_unsafe state2 event
      with e -> main_handle_error e (Printexc.get_backtrace ()) state2 (Some event)
    with e -> main_handle_error e (Printexc.get_backtrace ()) state (Some event)
  with e -> main_handle_error e (Printexc.get_backtrace ()) state None

and main_handle_unsafe (state: state) (event: event) : state =
begin
  match state, event with
  | Pre_init i_connect_params,
    Client_message (RequestMessage (id, InitializeRequest i_initialize_params)) ->
    let i_root = Lsp_helpers.get_root i_initialize_params |> Path.make in
    let d_ienv = {
      i_initialize_params;
      i_connect_params;
      i_root;
      i_version = get_current_version i_root;
      i_can_autostart_after_version_mismatch = true;
      i_server_id = 0;
      i_outstanding_local_requests = IdMap.empty;
      i_outstanding_local_handlers = IdMap.empty;
      i_outstanding_requests_from_server = WrappedMap.empty;
      i_isConnected = false;
    } in
    (* If the version in .flowconfig is simply incompatible with our current *)
    (* binary then it doesn't even make sense for us to start up. And future *)
    (* attempts by the client to launch us will fail as well. Clients which  *)
    (* receive the following response are expected to shut down their LSP.   *)
    let required_version = get_current_version i_root in
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
      d_start_time =  Unix.gettimeofday ();
      d_server_status = None;
      d_dialog_stopped = ShowMessageRequest.Absent;
      d_dialog_stopped_can_show = true;
      d_actionRequired_stopped = ActionRequired.Absent;
      d_dialog_connecting = ShowMessageRequest.Absent;
      d_dialog_connecting_can_show = true;
      d_progress_connecting = Progress.Absent;
      d_editor_open_files = SMap.empty;
    } in
    try_connect env

  | _, Client_message (RequestMessage (id, ShutdownRequest)) ->
    begin match state with Connected env -> close_conn env | _ -> () end;
    let response = ResponseMessage (id, ShutdownResult) in
    let json = Lsp_fmt.print_lsp response in
    to_stdout json;
    let _state = dismiss_ui state in
    Post_shutdown

  | _, Client_message (NotificationMessage ExitNotification) ->
    if state = Post_shutdown then lsp_exit_ok () else lsp_exit_bad ()

  | _, Client_message (NotificationMessage (CancelRequestNotification _)) ->
    state

  | Pre_init _, Client_message _ ->
    raise (Error.ServerNotInitialized "Server not initialized")

  | _, Client_message ((ResponseMessage (id, result)) as c) ->
    let ienv = match state with
      | Connected env -> env.c_ienv
      | Disconnected env -> env.d_ienv
      | _ -> failwith "Didn't expect an LSP response yet" in
    begin try
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
      | ShowMessageRequestResult result, ShowMessageHandler handle -> handle result state
      | ErrorResult (e, msg), _ -> handle_error (e, msg) state
      | _ -> failwith (Printf.sprintf "Response %s has mistyped handler" (message_to_string c))
    with Not_found ->
      match state with
      | Connected cenv ->
        let state = track_to_server state c in
        let wrapped = decode_wrapped id in (* only forward responses if they're to current server *)
        if wrapped.server_id = cenv.c_ienv.i_server_id then send_lsp_to_server cenv c;
        state
      | _ ->
        failwith (Printf.sprintf "Response %s has missing handler" (message_to_string c))
    end

  | Connected cenv, Client_message c ->
    let state = track_to_server state c in
    send_lsp_to_server cenv c;
    state

  | _, Client_message ((NotificationMessage (DidOpenNotification _)) as c)
  | _, Client_message ((NotificationMessage (DidChangeNotification _)) as c)
  | _, Client_message ((NotificationMessage (DidSaveNotification _)) as c)
  | _, Client_message ((NotificationMessage (DidCloseNotification _)) as c) ->
    let state = track_to_server state c in
    state

  | Disconnected _, Client_message c ->
    let state = track_to_server state c in
    let method_ = Lsp_fmt.message_to_string c in
    let e = Error.RequestCancelled ("Server not connected; can't handle " ^ method_) in
    let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
    main_handle_error e stack state (Some event)

  | Post_shutdown, Client_message _ ->
    raise (Error.RequestCancelled "Server shutting down")

  | Connected cenv, Server_message (Persistent_connection_prot.ServerExit exit_code) ->
    Connected { cenv with c_about_to_exit_code = Some exit_code; }

  | Connected cenv, Server_message (Persistent_connection_prot.LspFromServer outgoing) ->
    let state = track_from_server state outgoing in
    let outgoing = match outgoing with
      | RequestMessage (id, request) ->
        let wrapped = { server_id = cenv.c_ienv.i_server_id; message_id = id; } in
        RequestMessage (encode_wrapped wrapped, request)
      | _ -> outgoing
    in
    to_stdout (Lsp_fmt.print_lsp outgoing);
    state

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
    let add
        (severity: PublishDiagnostics.diagnosticSeverity option)
        (error: Errors.error)
        (all: PublishDiagnostics.diagnostic list SMap.t)
      : PublishDiagnostics.diagnostic list SMap.t =
      let error = Errors.Lsp_output.lsp_of_error error in
      let location = Flow_lsp_conversions.loc_to_lsp error.Errors.Lsp_output.loc ~default_uri in
      let uri = location.Lsp.Location.uri in
      let related_to_lsp (loc, relatedMessage) =
        let relatedLocation = Flow_lsp_conversions.loc_to_lsp loc ~default_uri in
        { Lsp.PublishDiagnostics.relatedLocation; relatedMessage; } in
      let diagnostic = { Lsp.PublishDiagnostics.
        range = location.Lsp.Location.range;
        severity;
        code = Lsp.PublishDiagnostics.StringCode error.Errors.Lsp_output.code;
        source = Some "Flow";
        message = error.Errors.Lsp_output.message;
        relatedLocations = List.map error.Errors.Lsp_output.relatedLocations ~f:related_to_lsp;
      } in
      SMap.add ~combine:List.append uri [diagnostic] all
    in
    (* First construct an SMap from uri to diagnostic list, which gathers together *)
    (* all the errors and warnings per uri *)
    let all = Errors.ErrorSet.fold (add (Some PublishDiagnostics.Error)) errors SMap.empty in
    let all = Errors.ErrorSet.fold (add (Some PublishDiagnostics.Warning)) warnings all
    in
    if cenv.c_is_rechecking then
      do_additional_diagnostics cenv all
    else
      do_replacement_diagnostics cenv all

  | Connected cenv, Server_message Persistent_connection_prot.StartRecheck ->
    show_recheck_progress { cenv with c_is_rechecking = true; }

  | Connected cenv, Server_message Persistent_connection_prot.EndRecheck ->
    show_recheck_progress { cenv with c_is_rechecking = false; }

  | Connected cenv, Server_message (Persistent_connection_prot.Please_hold status) ->
    let (server_status, watcher_status) = status in
    let c_server_status = (server_status, Some watcher_status) in
    show_recheck_progress { cenv with c_server_status; }

  | _, Server_message _ ->
    failwith (Printf.sprintf "Unexpected %s in state %s"
      (string_of_event event) (string_of_state state))

  | Disconnected env, Tick ->
    let state = try_connect env in
    state

  | _, Tick ->
    state
end

and main_handle_error
    (e: exn)
    (stack: string)
    (state: state)
    (event: event option)
  : state =
  let open Marshal_tools in
  match e with
  | Server_fatal_connection_exception _edata when state = Post_shutdown ->
    state

  | Server_fatal_connection_exception edata -> begin
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
    let stack = edata.stack ^ "---\n" ^ stack in
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
      d_start_time =  Unix.time ();
      d_server_status = None;
      d_dialog_stopped = ShowMessageRequest.Absent;
      d_dialog_stopped_can_show = true;
      d_actionRequired_stopped = ActionRequired.Absent;
      d_dialog_connecting = ShowMessageRequest.Absent;
      d_dialog_connecting_can_show = true;
      d_progress_connecting = Progress.Absent;
      d_editor_open_files = Option.value (get_editor_open_files state) ~default:SMap.empty;
    }
    in
    let _state = state |> dismiss_ui |> dismiss_tracks in
    let state = Disconnected env in
    state
    end

  | Client_recoverable_connection_exception edata ->
    let stack = edata.stack ^ "---\n" ^ stack in
    let report = Printf.sprintf "Client exception: %s\n%s" edata.message stack in
    Lsp_helpers.telemetry_error to_stdout report;
    state

  | Client_fatal_connection_exception edata ->
    (* TODO(ljw): log this to scuba; normally no one even attends to stderr *)
    let stack = edata.stack ^ "---\n" ^ stack in
    let report = Printf.sprintf "Client fatal exception: %s\n%s" edata.message stack in
    Printf.eprintf "%s" report;
    lsp_exit_bad ()

  | e -> begin
      let (code, message, _data) = get_error_info e in
      let text = Printf.sprintf "FlowLSP exception %s [%i]\n%s" message code stack in
      match event with
      | Some (Client_message (RequestMessage (id, _request))) ->
        let json = Lsp_fmt.print_lsp_response id (ErrorResult (e, stack)) in
        to_stdout json;
      | _ ->
        Lsp_helpers.telemetry_error to_stdout text
    end;
    state


let command = CommandSpec.command spec main
