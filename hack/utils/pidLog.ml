(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let log_oc = ref None

let init root =
  assert (!log_oc = None);
  log_oc := Some (open_out (Lock.lock_name root "pids"))

let log ?reason:(reason=None) pid =
  let reason = match reason with 
    | None -> "unknown"
    | Some s -> s in
  match !log_oc with
    | None -> failwith "Can't write pid to uninitialized pids log"
    | Some oc -> Printf.fprintf oc "%d\t%s\n%!" pid reason

exception FailedToGetPids

let get_pids root =
  try 
    let ic = open_in (Lock.lock_name root "pids") in
    let results = ref [] in
    begin try 
      while true do
        let row = input_line ic in
        if Str.string_match (Str.regexp "^\\([0-9]+\\)\t\\(.+\\)") row 0
        then
          let pid = int_of_string (Str.matched_group 1 row) in
          let reason = Str.matched_group 2 row in
          results := (pid, reason)::!results;
      done;
    with End_of_file -> () end;
    close_in ic;
    List.rev !results
  with Sys_error _ -> 
    raise FailedToGetPids
