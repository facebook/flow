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
 *   * Use the BSER protocol for enhanced performance
 *)

exception Watchman_error of string
exception Timeout

let debug = false

let crash_marker_path root =
  let root_name = Path.slash_escaped_string_of_path root in
  Filename.concat GlobalConfig.tmp_dir (spf ".%s.watchman_failed" root_name)

type client = {
  socket: in_channel * out_channel;
  logs: Hh_json.json list;
  subscriptions: (string, Hh_json.json) Hashtbl.t;
}

type env = {
  client: client;
  (* The path of the directory we are interested in getting updates for *)
  root: Path.t;
  (* The root that watchman is actually watching *)
  watch_root: string;
  (* The path of root relative to watch_root. See
   * https://facebook.github.io/watchman/docs/cmd/watch-project.html *)
  relative_path: string;
}

let make_client socket = {
  socket;
  logs = [];
  subscriptions = Hashtbl.create 0;
}

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

let with_crash_record_exn root source f =
  try f ()
  with e ->
    close_out @@ open_out @@ crash_marker_path root;
    Hh_logger.exc ~prefix:("Watchman " ^ source ^ ": ") e;
    raise e

let with_crash_record root source f =
  try
    with_crash_record_exn root source f
  with _ ->
    Exit_status.(exit Watchman_failed)

let with_crash_record_opt root source f =
  Option.try_with (fun () -> with_crash_record_exn root source f)

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

let expression_filter = [
  J.strlist ["type"; "f"];
  J.pred "not" @@ [
    J.pred "anyof" @@ [
      J.strlist ["dirname"; ".hg"];
      J.strlist ["dirname"; ".git"];
      J.strlist ["dirname"; ".svn"];
    ]
  ]
]

let subscribe root watch_root relative_path =
  let root = Path.to_string root in
  let open Hh_json in
  JSON_Array begin
    [JSON_String "subscribe"; JSON_String watch_root; JSON_String root] @ [
      JSON_Object [
        "fields", J.strlist ["name"];
        "expression", J.pred "allof" expression_filter;
        "relative_root", JSON_String relative_path;
      ]
    ]
  end

let read_with_timeout timeout ic =
  Sys_utils.with_timeout timeout
    ~do_:(fun () -> input_line ic)
    ~on_timeout:begin fun _ ->
      EventLogger.watchman_timeout ();
      raise Timeout
    end

let watch_project root = J.strlist ["watch-project"; Path.to_string @@ root]

let has_update env =
  let ic, _oc = env.client.socket in
  let in_fd = Unix.descr_of_in_channel ic in
  let ready_fd_l, _, _ = Unix.select [in_fd] [] [] 0.0 in
  ready_fd_l <> []

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

let receive ?(timeout=120) client =
  let ic, _oc = client.socket in
  let output = read_with_timeout timeout ic in
  if debug then Printf.eprintf "Watchman response: %s\n%!" output;
  let response =
    try Hh_json.json_of_string output
    with e ->
      Printf.eprintf "Failed to parse string as JSON: %s\n%!" output;
      raise e
  in
  assert_no_error response;
  let subscription =
    Option.try_with @@ fun () -> J.get_string_val "subscription" response in
  match subscription with
  | Some sub ->
    Hashtbl.add client.subscriptions sub response;
    None
  | None -> Some response

let extract_file_names env json =
  let files = J.get_array_val "files" json in
  let files = List.map files begin fun json ->
    let s = Hh_json.get_string_exn json in
    Filename.(concat env.watch_root @@ concat env.relative_path s)
  end in
  files

let process_subscriptions env ~init ~f =
  let root = Path.to_string env.root in
  let subs =
    Hashtbl.find_all env.client.subscriptions root in
  while Hashtbl.mem env.client.subscriptions root do
    Hashtbl.remove env.client.subscriptions root
  done;
  List.fold_left subs ~init ~f

(* The first time this is called, it will return all the files in the repo.
 * Subsequent times will just return the files that have changed in the
 * interim. *)
let get_changes env =
  with_crash_record env.root "get_changes" @@ fun () ->
  assert (receive env.client = None);
  process_subscriptions env
    ~init:SSet.empty
    ~f:begin fun acc response ->
      let set = set_of_list @@ extract_file_names env response in
      SSet.union set acc
    end

let poll_changes env =
  if has_update env
  then get_changes env
  else SSet.empty

let exec ?(timeout=120) client json =
  let _ic, oc = client.socket in
  let json_str = Hh_json.(json_to_string json) in
  if debug then Printf.eprintf "Watchman query: %s\n%!" json_str;
  output_string oc json_str;
  output_string oc "\n";
  flush oc;
  let response = ref @@ receive ~timeout client in
  while Option.is_none !response do
    response := receive ~timeout client
  done;
  unsafe_opt !response

let get_sockname timeout =
  let ic = Unix.open_process_in "watchman get-sockname --no-pretty" in
  let output = read_with_timeout timeout ic in
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  let json = Hh_json.json_of_string output in
  J.get_string_val "sockname" json

let init timeout root =
  with_crash_record_opt root "init" @@ fun () ->
  let sockname = get_sockname timeout in
  let socket = Unix.(open_connection (ADDR_UNIX sockname)) in
  let client = make_client socket in
  ignore @@ exec client (
    capability_check ["cmd-watch-project"; "cmd-subscribe"; "relative_root"]);
  let response = exec client (watch_project root) in
  let watch_root = J.get_string_val "watch" response in
  let relative_path = J.get_string_val "relative_path" ~default:"" response in
  ignore @@ exec client (subscribe root watch_root relative_path);
  let env = {
    client;
    root;
    watch_root;
    relative_path;
  } in
  env
