(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Initializes the unix domain socket *)
let unix_socket sock_name =
  try
    Sys_utils.with_umask 0o111 begin fun () ->
      Sys_utils.mkdir_no_fail (Filename.dirname sock_name);
      if Sys.file_exists sock_name then Sys.remove sock_name;
      let domain, addr =
        if Sys.win32 then
          Unix.(PF_INET, Unix.ADDR_INET (inet_addr_loopback, 0))
        else
          Unix.(PF_UNIX, Unix.ADDR_UNIX sock_name) in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
      let _ = Unix.setsockopt sock Unix.SO_REUSEADDR true in
      let _ = Unix.bind sock addr in
      let _ = Unix.listen sock 10 in
      let () =
        match Unix.getsockname sock with
        | Unix.ADDR_UNIX _ -> ()
        | Unix.ADDR_INET (_, port) ->
            let oc = open_out_bin sock_name in
            output_binary_int oc port;
            close_out oc in
      sock
    end
  with Unix.Unix_error (err, _, _) ->
    Printf.fprintf stderr "%s\n" (Unix.error_message err);
    exit 1

(* So the sockaddr_un structure puts a strict limit on the length of a socket
  * address. This appears to be 104 chars on mac os x and 108 chars on my
  * centos box. *)
let max_addr_length = 103
let min_name_length = 17

let get_path path =
  let dir = (Filename.dirname path)^"/" in
  let filename = Filename.basename path in
  let root_part = Filename.chop_extension filename in
  let root_length = String.length root_part in
  let extension_length = String.length filename - root_length in
  let extension = String.sub filename root_length extension_length in

  (* It's possible that the directory path is too long. If so, let's give up and
   * use /tmp/ *)
  let dir =
    if String.length dir > max_addr_length - min_name_length
    then Filename.get_temp_dir_name ()
    else dir in
  let max_root_part_length =
    max_addr_length - (String.length dir) - extension_length in
  let root_part =
    if root_length > max_root_part_length
    then begin
      let len = String.length root_part in
      let prefix = String.sub root_part 0 5 in
      let suffix = String.sub root_part (len - 5) 5 in
      let digest = Digest.to_hex (Digest.string root_part) in
      (* 5 char prefix + 5 char suffix + 2 periods *)
      let max_digest_length = max_root_part_length - 12 in
      let digest_part = if String.length digest > max_digest_length
        then String.sub digest 0 max_digest_length
        else digest in
      prefix ^ "." ^ digest_part ^ "." ^ suffix
    end else root_part in
  Filename.concat dir (Printf.sprintf "%s%s" root_part extension)

let init_unix_socket www_root_path =
  unix_socket (get_path www_root_path)
