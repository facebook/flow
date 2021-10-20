(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type proc_stat = {
  cmdline: string;
  ppid: int;
}

let cmdline_delimiter_re = Str.regexp "\x00"

(* Takes a PID and returns the full command line the process was started with *)
let get_cmdline (pid : int) : (string, string) result =
  (* NOTE: Linux's OS type is Unix *)
  if Sys.os_type <> "Unix" then
    Error "Getting cmdline is not implemented for non-Unix OS types"
  else
    let cmdline_path = Printf.sprintf "/proc/%d/cmdline" pid in
    try
      let line = Str.global_replace cmdline_delimiter_re " " (Disk.cat cmdline_path) in
      Ok line
    with
    | e ->
      let error =
        Printf.sprintf "No 'cmdline' file found for PID %d: '%s'" pid (Printexc.to_string e)
      in
      Error error

(* Takes a PID and returns the information about the process, including
   the name and the PID of the parent process (PPID) *)
let get_proc_stat (pid : int) : (proc_stat, string) result =
  (* NOTE: Linux's OS type is Unix *)
  if Sys.os_type <> "Unix" then
    Error "Getting cmdline is not implemented for non-Unix OS types"
  else
    let stat_path = Printf.sprintf "/proc/%d/stat" pid in
    try
      let stat = Scanf.Scanning.from_string (Disk.cat stat_path) in
      try
        let record =
          Scanf.bscanf
            stat
            "%d (%s@) %c %d"
            (fun _my_pid _comm _state ppid : (proc_stat, string) result ->
              match get_cmdline pid with
              | Ok cmdline -> Ok { cmdline; ppid }
              | Error err -> Error err
          )
        in
        record
      with
      | e ->
        let error =
          Printf.sprintf "Error reading 'stat' for PID %d: %s" pid (Printexc.to_string e)
        in
        Error error
    with
    | e ->
      let error =
        Printf.sprintf "No 'stat' file found for PID %d: '%s'" pid (Printexc.to_string e)
      in
      Error error

let get_proc_stack ?(max_depth : int = -1) ?(max_length : int = max_int) (pid : int) :
    (string list, string) result =
  let prepare_cmdline (cmdline : string) : string =
    let cmdline = String.trim cmdline in
    if max_length >= String.length cmdline then
      cmdline
    else
      String.trim (String.sub cmdline 0 max_length) ^ "..."
  in
  (* We could have max_depth as optional, but then everybody would have to pass in None *)
  (* let max_depth = match max_depth with | None -> -1 | Some max_depth -> max_depth in *)
  let rec build_proc_stack (curr_pid : int) (proc_stack : string list) (counter : int) :
      (string list, string) result =
    if curr_pid = 0 then
      Ok proc_stack
    else if counter = max_depth then
      Ok proc_stack
    else
      match get_proc_stat curr_pid with
      | Ok stat ->
        build_proc_stack stat.ppid (prepare_cmdline stat.cmdline :: proc_stack) (counter + 1)
      | Error e -> Error e
  in
  build_proc_stack pid [] 0
