(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Result.Monad_infix

let spf = Printf.sprintf

let read_proc_file filename pid =
  let file = spf "/proc/%d/%s" pid filename in
  try Ok (Sys_utils.cat file) with
  | exn ->
    let exn = Exception.wrap exn in
    Error (Exception.to_string exn)

let parse_cgroup raw_cgroup_contents =
  match String.split raw_cgroup_contents ~on:'\n' with
  | [] -> Error "Expected at least one cgroup in /proc/<PID>/cgroup file"
  | first_line :: _ -> begin
    match String.split first_line ~on:':' with
    | [_id; _controllers; cgroup] -> Ok cgroup
    | _ -> Error "First line of  /proc/<PID>/cgroup file was not correctly formatted"
  end

let asset_procfs_supported =
  let memoized_result = ref None in
  fun () ->
    match !memoized_result with
    | Some supported -> supported
    | None ->
      let supported =
        if Sys.unix && Stdlib.Sys.file_exists "/proc" then
          Ok ()
        else
          Error "Proc filesystem not supported"
      in
      memoized_result := Some supported;
      supported

(* In cgroup v1 a pid can be in multiple cgroups. In cgroup v2 it will only be in a single cgroup.
 *)
let first_cgroup_for_pid pid =
  asset_procfs_supported () >>= fun () -> read_proc_file "cgroup" pid >>= parse_cgroup
