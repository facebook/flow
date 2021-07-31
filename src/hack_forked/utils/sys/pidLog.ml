(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

let log_oc = ref None

let enabled = ref true

let disable () = enabled := false

let init pids_file =
  assert (!log_oc = None);
  Sys_utils.with_umask 0o111 (fun () ->
      Sys_utils.mkdir_no_fail (Filename.dirname pids_file);
      let oc = open_out pids_file in
      log_oc := Some oc;
      Unix.(set_close_on_exec (descr_of_out_channel oc)))

let log ?reason ?(no_fail = false) pid =
  if !enabled then
    let pid = Sys_utils.pid_of_handle pid in
    let reason =
      match reason with
      | None -> "unknown"
      | Some s -> s
    in
    match !log_oc with
    | None when no_fail -> ()
    | None -> failwith "Can't write pid to uninitialized pids log"
    | Some oc -> Printf.fprintf oc "%d\t%s\n%!" pid reason

exception FailedToGetPids

let get_pids pids_file =
  try
    let ic = open_in pids_file in
    let results = ref [] in
    begin
      try
        while true do
          let row = input_line ic in
          if Str.string_match (Str.regexp "^\\([0-9]+\\)\t\\(.+\\)") row 0 then
            let pid = int_of_string (Str.matched_group 1 row) in
            let reason = Str.matched_group 2 row in
            results := (pid, reason) :: !results
        done
      with
      | End_of_file -> ()
    end;
    close_in ic;
    List.rev !results
  with
  | Sys_error _ -> raise FailedToGetPids

let close () =
  Base.Option.iter !log_oc ~f:close_out;
  log_oc := None
