(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel
open Result.Monad_infix

let spf = Printf.sprintf

let read_proc_file filename pid =
  let file = spf "/proc/%d/%s" pid filename in
  try Ok (Sys_utils.cat file)
  with exn ->
    let exn = Exception.wrap exn in
    Error (Exception.to_string exn)

type status = {
  (* The total number of bytes currently in memory used for anonymous memory *)
  rss_anon: int;
  (* The total number of bytes currently in memory used for file mappings *)
  rss_file: int;
  (* The total number of bytes currently in memory used for shared memory *)
  rss_shmem: int;
  (* The total number of bytes currently in memory. It should be the sum of
   * rss_anon + rss_file + rss_shmem *)
  rss_total: int;
  (* The high water mark for the number of bytes in memory at one time *)
  rss_hwm: int;
}

(* The stats we're reading always end in kB. If we start reading more stats then we'll need to beef
 * up this logic *)
let humanReadableToBytes str =
  (try Scanf.sscanf str "%d kB" (fun kb -> 1000 * kb) with _ -> 0)

let parse_status raw_status_contents =
  let stats =
    String.split raw_status_contents ~on:'\n'
    |> List.fold_left ~init:SMap.empty ~f:(fun stats line ->
           match String.split line ~on:':' with
           | [raw_key; raw_stat] ->
             let key = String.strip raw_key in
             let stat = String.strip raw_stat in
             SMap.add key stat stats
           | _ -> stats)
  in
  {
    rss_anon =
      SMap.find_opt "RssAnon" stats
      |> Option.value_map ~default:0 ~f:humanReadableToBytes;
    rss_file =
      SMap.find_opt "RssFile" stats
      |> Option.value_map ~default:0 ~f:humanReadableToBytes;
    rss_shmem =
      SMap.find_opt "RssShmem" stats
      |> Option.value_map ~default:0 ~f:humanReadableToBytes;
    rss_total =
      SMap.find_opt "VmRSS" stats
      |> Option.value_map ~default:0 ~f:humanReadableToBytes;
    rss_hwm =
      SMap.find_opt "VmHWM" stats
      |> Option.value_map ~default:0 ~f:humanReadableToBytes;
  }

let parse_cgroup raw_cgroup_contents =
  match String.split raw_cgroup_contents ~on:'\n' with
  | [] -> Error "Expected at least one cgroup in /proc/<PID>/cgroup file"
  | first_line :: _ ->
    begin
      match String.split first_line ~on:':' with
      | [_id; _controllers; cgroup] -> Ok cgroup
      | _ ->
        Error
          "First line of  /proc/<PID>/cgroup file was not correctly formatted"
    end

let asset_procfs_supported =
  let memoized_result = ref None in
  fun () ->
    match !memoized_result with
    | Some supported -> supported
    | None ->
      let supported =
        if Sys.unix && Sys.file_exists "/proc" then
          Ok ()
        else
          Error "Proc filesystem not supported"
      in
      memoized_result := Some supported;
      supported

let status_for_pid pid =
  asset_procfs_supported () >>= fun () ->
  read_proc_file "status" pid >>| parse_status

(* In cgroup v1 a pid can be in multiple cgroups. In cgroup v2 it will only be in a single cgroup.
 *)
let first_cgroup_for_pid pid =
  asset_procfs_supported () >>= fun () ->
  read_proc_file "cgroup" pid >>= parse_cgroup
