(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let default_flowconfig_name = ".flowconfig"

let add_dir_sep dir =
  Filename.(
    if check_suffix dir dir_sep then
      dir
    else
      dir ^ dir_sep)

let mk_root flowconfig_name root =
  if flowconfig_name = default_flowconfig_name then
    root
  else
    Path.concat root flowconfig_name

let digest_root_part root_part max_len =
  let len = String.length root_part in
  if len <= max_len then
    root_part
  else
    let prefix = String.sub root_part 0 5 in
    let suffix = String.sub root_part (len - 5) 5 in
    let digest = OpaqueDigest.to_hex (OpaqueDigest.string root_part) in
    (* 5 char prefix + 5 char suffix + 2 underscores *)
    let max_digest_length = max_len - 12 in
    let digest_part =
      if String.length digest > max_digest_length then
        String.sub digest 0 max_digest_length
      else
        digest
    in
    Printf.sprintf "%s_%s_%s" prefix digest_part suffix

let file_of_root ?max_root_part_len extension ~flowconfig_name ~tmp_dir root =
  let tmp_dir = tmp_dir |> Path.make |> Path.to_string |> add_dir_sep in
  let root = mk_root flowconfig_name root in
  let root_part = Path.slash_escaped_string_of_path root in
  let root_part =
    match max_root_part_len with
    | None -> root_part
    | Some max_root_part_len -> digest_root_part root_part max_root_part_len
  in
  Printf.sprintf "%s%s.%s" tmp_dir root_part extension

let config_file flowconfig_name root = Path.to_string (Path.concat root flowconfig_name)

(* Generating really long filenames can hit some limits. For example
 *
 * /* /usr/include/linux/limits.h */
 * #define NAME_MAX         255    /* # chars in a file name */
 *
 * Which can cause ENAMETOOLONG or stuff like that. So let's cap our filenames (without extensions)
 * at 200 characters *)
let max_root_part_len = 200

let log_file = file_of_root ~max_root_part_len "log"

let monitor_log_file = file_of_root ~max_root_part_len "monitor_log"

let lock_file = file_of_root ~max_root_part_len "lock"

let pids_file = file_of_root ~max_root_part_len "pids"

let recheck_stats_file = file_of_root ~max_root_part_len "recheck_stats"

(* Socket files don't care about length. socket.ml will worry about abridging those *)
let socket_file = file_of_root "sockv3"

let legacy2_socket_file = file_of_root "sockv2"

let legacy1_socket_file = file_of_root "sock"
