(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **)

open Core
open Utils

(*
 * Module for us to interface with Watchman, a file watching service.
 * https://facebook.github.io/watchman/
 *
 * TODO:
 *   * Connect directly to the Watchman server socket instead of spawning
 *     a client process each time
 *   * Use the BSER protocol for enhanced performance
 *)

exception Watchman_error of string
exception Timeout

let debug = false

let crash_marker_path root =
  let root_name = Path.slash_escaped_string_of_path root in
  Filename.concat GlobalConfig.tmp_dir (spf ".%s.watchman_failed" root_name)

type init_settings = {
  subscribe_to_changes: bool;
  (** Seconds used for init timeout - will be reused for reinitialization. *)
  init_timeout: int;
  root: Path.t;
}

type dead_env = {
  (** Will reuse original settings to reinitializing watchman subscription. *)
  prior_settings : init_settings;
  reinit_attempts: int;
  dead_since: float;
}

type env = {
  settings : init_settings;
  socket: Timeout.in_channel * out_channel;
  watch_root: string;
  relative_path: string;
  (* See https://facebook.github.io/watchman/docs/clockspec.html *)
  mutable clockspec: string;
}

let dead_env_from_alive env =
  {
    prior_settings = env.settings;
    dead_since = Unix.time ();
    reinit_attempts = 0;
  }

type watchman_instance =
  (** Indicates a dead watchman instance (most likely due to chef upgrading,
   * reconfiguration, or a user terminating watchman) detected by,
   * for example, a pipe error.
   *
   * TODO: Currently fallback to a Watchman_dead is only handled in calls
   * wrapped by the with_crash_record. Pipe errors elsewhere (for example
   * during exec) will still result in Hack exiting. Need to cover those
   * cases too. *)
  | Watchman_dead of dead_env
  | Watchman_alive of env

(* Some JSON processing helpers *)
module J = struct
  let try_get_val key json =
    let obj = Hh_json.get_object_exn json in
    List.Assoc.find obj key

  let get_string_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Hh_json.get_string_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let get_array_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Hh_json.get_array_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let strlist args =
    Hh_json.JSON_Array begin
      List.map args (fun arg -> Hh_json.JSON_String arg)
    end

  (* Prepend a string to a JSON array of strings. pred stands for predicate,
   * because that's how they are typically represented in watchman. See e.g.
   * https://facebook.github.io/watchman/docs/expr/allof.html *)
  let pred name args =
    let open Hh_json in
    JSON_Array (JSON_String name :: args)
end

(*****************************************************************************)
(* JSON methods. *)
(*****************************************************************************)

let clock root = J.strlist ["clock"; root]

type watch_command = Subscribe | Query

let request_json
    ?(extra_kv=[]) ?(extra_expressions=[]) watchman_command env =
  let open Hh_json in
  let command = begin match watchman_command with
    | Subscribe -> "subscribe"
    | Query -> "query" end in
  let header =
    [JSON_String command ; JSON_String env.watch_root] @
      begin
        match watchman_command with
        | Subscribe -> [JSON_String "hh_type_check_watcher"]
        | _ -> []
      end in
  let directives = [
    JSON_Object (extra_kv @ [
      "fields", J.strlist ["name"];
      "relative_root", JSON_String env.relative_path;
      "expression", J.pred "allof" @@ (extra_expressions @ [
        J.strlist ["type"; "f"];
        J.pred "anyof" @@ [
          J.strlist ["name"; ".hhconfig"];
          J.pred "anyof" @@ [
            J.strlist ["suffix"; "php"];
            J.strlist ["suffix"; "phpt"];
            J.strlist ["suffix"; "hh"];
            J.strlist ["suffix"; "hhi"];
            J.strlist ["suffix"; "xhp"];
            (* FIXME: This is clearly wrong, but we do it to match the
             * behavior on the server-side. We need to investigate if
             * tracking js files is truly necessary.
             *)
            J.strlist ["suffix"; "js"];
          ];
        ];
        J.pred "not" @@ [
          J.pred "anyof" @@ [
            J.strlist ["dirname"; ".hg"];
            J.strlist ["dirname"; ".git"];
            J.strlist ["dirname"; ".svn"];
          ]
        ]
      ])
    ])
  ] in
  let request = JSON_Array (header @ directives) in
  request

let all_query env =
  request_json
    ~extra_expressions:([Hh_json.JSON_String "exists"])
    Query env

let since_query env =
  request_json
    ~extra_kv: ["since", Hh_json.JSON_String env.clockspec]
    Query env

let subscribe env = request_json
                      ~extra_kv:["since", Hh_json.JSON_String env.clockspec ;
                                 "defer", J.strlist ["hg.update"]]
                      Subscribe env

let watch_project root = J.strlist ["watch-project"; root]

(* See https://facebook.github.io/watchman/docs/cmd/version.html *)
let capability_check ?(optional=[]) required =
  let open Hh_json in
  JSON_Array begin
    [JSON_String "version"] @ [
      JSON_Object [
        "optional", J.strlist optional;
        "required", J.strlist required;
      ]
    ]
  end

(*****************************************************************************)
(* Handling requests and responses. *)
(*****************************************************************************)

let read_with_timeout timeout ic =
   Timeout.with_timeout ~timeout
     ~do_:(fun t -> Timeout.input_line ~timeout:t ic)
     ~on_timeout:begin fun _ ->
                   EventLogger.watchman_timeout ();
                   raise Timeout
                 end

let assert_no_error obj =
  (try
     let warning = J.get_string_val "warning" obj in
     EventLogger.watchman_warning warning;
     Hh_logger.log "Watchman warning: %s\n" warning
   with Not_found -> ());
  (try
     let error = J.get_string_val "error" obj in
     EventLogger.watchman_error error;
     raise @@ Watchman_error error
   with Not_found -> ())

let sanitize_watchman_response output =
  if debug then Printf.eprintf "Watchman response: %s\n%!" output;
  let response =
    try Hh_json.json_of_string output
    with e ->
      Printf.eprintf "Failed to parse string as JSON: %s\n%!" output;
      raise e
  in
  assert_no_error response;
  response

let exec ?(timeout=120) (ic, oc) json =
  let json_str = Hh_json.(json_to_string json) in
  if debug then Printf.eprintf "Watchman request: %s\n%!" json_str ;
  output_string oc json_str;
  output_string oc "\n";
  flush oc ;
  sanitize_watchman_response (read_with_timeout timeout ic)

(*****************************************************************************)
(* Initialization, reinitialization, and crash-tracking. *)
(*****************************************************************************)

let get_sockname timeout =
  let ic =
    Timeout.open_process_in "watchman"
    [| "watchman"; "get-sockname"; "--no-pretty" |] in
  let output = read_with_timeout timeout ic in
  assert (Timeout.close_process_in ic = Unix.WEXITED 0);
  let json = Hh_json.json_of_string output in
  J.get_string_val "sockname" json

let with_crash_record_exn root source f =
  try f ()
  with e ->
    close_out @@ open_out @@ crash_marker_path root;
    Hh_logger.exc ~prefix:("Watchman " ^ source ^ ": ") e;
    raise e

let with_crash_record_opt root source f =
  Option.try_with (fun () -> with_crash_record_exn root source f)

let init { init_timeout; subscribe_to_changes; root } =
  with_crash_record_opt root "init" @@ fun () ->
  let root_s = Path.to_string root in
  let sockname = get_sockname init_timeout in
  let socket = Timeout.open_connection (Unix.ADDR_UNIX sockname) in
  ignore @@ exec socket (capability_check ["relative_root"]);
  let response = exec socket (watch_project root_s) in
  let watch_root = J.get_string_val "watch" response in
  let relative_path = J.get_string_val "relative_path" ~default:"" response in

  let clockspec =
    exec socket (clock watch_root) |> J.get_string_val "clock" in
  let env = {
    settings = {
      init_timeout;
      subscribe_to_changes;
      root;
    };
    socket;
    watch_root;
    relative_path;
    clockspec;
  } in
  if subscribe_to_changes then (ignore @@ exec env.socket (subscribe env)) ;
  Watchman_alive env

let with_crash_record instance source ~fallback_value f =
  match instance with
  | Watchman_dead _ ->
    instance, fallback_value
  | Watchman_alive env -> begin
    try
      instance, with_crash_record_exn env.settings.root source (fun () -> f env)
    with
      | Sys_error("Broken pipe") ->
        Hh_logger.log "Watchman Pipe broken.";
        EventLogger.watchman_died_caught ();
        Watchman_dead (dead_env_from_alive env), fallback_value
      | Sys_error("Connection reset by peer") ->
        Hh_logger.log "Watchman connection reset by peer.";
        EventLogger.watchman_died_caught ();
        Watchman_dead (dead_env_from_alive env), fallback_value
      | e ->
        let msg = Printexc.to_string e in
        EventLogger.watchman_uncaught_failure msg;
        Exit_status.(exit Watchman_failed)
  end

let poll_for_updates env =
  (* Use the timeout mechanism to manage our polling frequency. *)
  let timeout = 0 in
  try
    let output = begin
      let in_channel, _  = env.socket in
      Timeout.with_timeout
        ~do_: (fun t -> Timeout.input_line ~timeout:t in_channel)
        ~timeout
        ~on_timeout:begin fun _ -> () end
    end in
    sanitize_watchman_response output
  with
  | Timeout.Timeout ->
    let clockspec = begin
        exec env.socket (clock env.watch_root) |>
          J.get_string_val "clock" end in
    let timeout_str = "{\"files\":[]," ^ "\"clock\":\"" ^ clockspec ^ "\"}" in
    Hh_json.json_of_string (timeout_str)
  | _ as e ->
    raise e

let extract_file_names env json =
  let files = J.get_array_val "files" json in
  let files = List.map files begin fun json ->
    let s = Hh_json.get_string_exn json in
    let abs =
      Filename.concat env.watch_root @@
      Filename.concat env.relative_path s in
    abs
  end in
  files

let within_backoff_time attempts time =
  let offset = 4.0 *. (2.0 ** float (if attempts > 3 then 3 else attempts)) in
  (Unix.time ()) >= time +. offset

let maybe_restart_instance instance = match instance with
  | Watchman_alive _ -> instance
  | Watchman_dead dead_env ->
    if within_backoff_time dead_env.reinit_attempts dead_env.dead_since then
      let () =
        Hh_logger.log "Attemping to reestablish watchman subscription" in
      match init dead_env.prior_settings with
      | None ->
        Hh_logger.log "Reestablishing watchman subscription failed.";
        EventLogger.watchman_connection_reestablishment_failed ();
        Watchman_dead { dead_env with
          reinit_attempts = dead_env.reinit_attempts + 1 }
      | Some (Watchman_dead _) ->
        Hh_logger.log "Watchman subscription failed during reestablishment";
        EventLogger.watchman_connection_reestablishment_failed ();
        Watchman_dead { dead_env with
          reinit_attempts = dead_env.reinit_attempts + 1 }
      | Some (Watchman_alive env) ->
        Hh_logger.log "Watchman connection reestablished.";
        EventLogger.watchman_connection_reestablished ();
        Watchman_alive env
    else
      instance

let get_all_files instance =
  let instance = maybe_restart_instance instance in
  with_crash_record instance "get_all_files" ~fallback_value:[] @@ fun env ->
    let response = exec env.socket (all_query env) in
    env.clockspec <- J.get_string_val "clock" response;
    extract_file_names env response

let get_changes instance =
  let instance = maybe_restart_instance instance in
  with_crash_record instance "get_changes"
    ~fallback_value:SSet.empty @@ fun env ->
    let response = begin
        if env.settings.subscribe_to_changes then poll_for_updates env
        else exec env.socket (since_query env)
    end in
    (* The subscription doesn't use the clockspec post-initialization, but it
     * may be useful to keep this info around.
     *)
    env.clockspec <- J.get_string_val "clock" response;
    set_of_list @@ extract_file_names env response
