(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

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

type env = {
  sockname: string;
  root: Path.t;
  watch_root: string;
  relative_path: string;
  (* See https://facebook.github.io/watchman/docs/clockspec.html *)
  mutable clockspec: string;
}

(* Some JSON processing helpers *)
module J = struct
  let try_get_val key json =
    let obj = Json.get_object_exn json in
    List.Assoc.find obj key

  let get_string_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Json.get_string_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let get_array_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Json.get_array_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let strlist args =
    Json.JSON_Array begin
      List.map args (fun arg -> Json.JSON_String arg)
    end

  (* Prepend a string to a JSON array of strings. pred stands for predicate,
   * because that's how they are typically represented in watchman. See e.g.
   * https://facebook.github.io/watchman/docs/expr/allof.html *)
  let pred name args =
    let open Json in
    JSON_Array (JSON_String name :: args)
end

let with_crash_record root f =
  try f ()
  with e ->
    close_out @@ open_out @@ crash_marker_path root;
    raise e

let assert_no_error obj =
  try
    let warning = J.get_string_val "warning" obj in
    EventLogger.watchman_warning warning;
    Hh_logger.log "Watchman warning: %s\n" warning
  with Not_found -> ();
  try
    let error = J.get_string_val "error" obj in
    EventLogger.watchman_error error;
    raise @@ Watchman_error error
  with Not_found -> ()

let clock root = J.strlist ["clock"; root]

let base_query
    ?(extra_kv=[]) ?(extra_expressions=[]) ?(sync_timeout=300_000) env =
  let open Json in
  JSON_Array begin
    [JSON_String "query"; JSON_String env.watch_root] @ [
      JSON_Object (extra_kv @ [
        "sync_timeout", Json.int_ sync_timeout;
        "fields", J.strlist ["name"];
        "relative_root", JSON_String env.relative_path;
        "expression", J.pred "allof" @@ (extra_expressions @ [
          J.strlist ["type"; "f"];
          J.pred "not" @@ [
            J.pred "anyof" @@ [
              J.strlist ["dirname"; ".hg"];
              J.strlist ["dirname"; ".git"];
              J.strlist ["dirname"; ".svn"];
            ]
          ]
        ])
      ])
    ]
  end

let query env = base_query ~extra_expressions:([Json.JSON_String "exists"]) env

let since env =
  base_query ~extra_kv:["since", Json.JSON_String env.clockspec] env

let watch_project root = J.strlist ["watch-project"; root]

(* See https://facebook.github.io/watchman/docs/cmd/version.html *)
let capability_check ?(optional=[]) required =
  let open Json in
  JSON_Array begin
    [JSON_String "version"] @ [
      JSON_Object [
        "optional", J.strlist optional;
        "required", J.strlist required;
      ]
    ]
  end

let read_with_timeout timeout ic =
  Timeout.with_timeout ~timeout
    ~do_:(fun t -> Timeout.input_line ~timeout:t ic)
    ~on_timeout:begin fun _ ->
      EventLogger.watchman_timeout ();
      raise Timeout
    end

let exec ?(timeout=120) sockname json =
  let ic, oc = Timeout.open_connection (Unix.ADDR_UNIX sockname) in
  let json_str = Json.(json_to_string json) in
  if debug then Printf.eprintf "Watchman query: %s\n%!" json_str;
  output_string oc json_str;
  output_string oc "\n";
  flush oc;
  let output = read_with_timeout timeout ic in
  Timeout.close_in ic;
  if debug then Printf.eprintf "Watchman response: %s\n%!" output;
  let response =
    try Json.json_of_string output
    with e ->
      Printf.eprintf "Failed to parse string as JSON: %s\n%!" output;
      raise e
  in
  assert_no_error response;
  response

let extract_file_names env json =
  let files = J.get_array_val "files" json in
  let files = List.map files begin fun json ->
    let s = Json.get_string_exn json in
    let abs =
      Filename.concat env.watch_root @@
      Filename.concat env.relative_path s in
    abs
  end in
  files

let get_all_files env =
  with_crash_record env.root @@ fun () ->
  let response = exec env.sockname (query env) in
  extract_file_names env response

let get_changes env =
  with_crash_record env.root @@ fun () ->
  let response = exec env.sockname (since env) in
  env.clockspec <- J.get_string_val "clock" response;
  set_of_list @@ extract_file_names env response

let get_sockname timeout =
  let ic =
    Timeout.open_process_in "watchman" [|"watchman"; "get-sockname"; "--no-pretty" |] in
  let output = read_with_timeout timeout ic in
  assert (Timeout.close_process_in ic = Unix.WEXITED 0);
  let json = Json.json_of_string output in
  J.get_string_val "sockname" json

let init timeout root =
  with_crash_record root @@ fun () ->
  let root_s = Path.to_string root in
  let sockname = get_sockname timeout in
  ignore @@ exec sockname (capability_check ["relative_root"]);
  let response = exec sockname (watch_project root_s) in
  let watch_root = J.get_string_val "watch" response in
  let relative_path = J.get_string_val "relative_path" ~default:"" response in
  let clockspec = exec sockname (clock watch_root)
    |> J.get_string_val "clock" in
  let env = {
    sockname;
    root;
    watch_root;
    relative_path;
    clockspec;
  } in
  env
