(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type addr =
  | Inet of Unix.inet_addr * int
  | Unix of string

(* On Linux/Mac/BSD, sockaddr_un.sun_path is a fixed length. To handle longer paths,
   we chdir to that directory and use a relative path instead. The callback provides
   a Unix.sockaddr with a relative path that you can use to bind or read from. Perform
   as little as possible within the callback, since it has an unexpected working dir.
   This function tries to make it awkward for the Unix.sockaddr with the relative path
   to escape from the callback. *)
let with_addr addr f =
  let cwd = Sys.getcwd () in
  try
    let sockaddr =
      match addr with
      | Inet (inet, port) -> Unix.ADDR_INET (inet, port)
      | Unix file ->
        let dir = Filename.dirname file in
        let base = Filename.basename file in
        let () = Sys.chdir dir in
        Unix.ADDR_UNIX (Filename.concat "." base)
    in
    let result = f sockaddr in
    let () = Sys.chdir cwd in
    result
  with exn ->
    let exn = Exception.wrap exn in
    let () =
      match addr with
      | Unix _ -> Sys.chdir cwd
      | Inet _ -> ()
    in
    Exception.reraise exn

(* Initializes the unix domain socket *)
let unix_socket sock_name =
  try
    Sys_utils.with_umask 0o111 (fun () ->
        Sys_utils.mkdir_no_fail (Filename.dirname sock_name);
        if Sys.file_exists sock_name then Sys.remove sock_name;
        let (domain, addr) =
          if Sys.win32 then
            Unix.(PF_INET, Inet (inet_addr_loopback, 0))
          else
            Unix.(PF_UNIX, Unix sock_name)
        in
        let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
        let () = Unix.set_close_on_exec sock in
        let () = Unix.setsockopt sock Unix.SO_REUSEADDR true in
        let () = with_addr addr @@ Unix.bind sock in
        let () = Unix.listen sock 10 in
        let () =
          match Unix.getsockname sock with
          | Unix.ADDR_UNIX _ -> ()
          | Unix.ADDR_INET (_, port) ->
            let oc = open_out_bin sock_name in
            output_binary_int oc port;
            close_out oc
        in
        sock)
  with Unix.Unix_error (err, _, _) ->
    Printf.eprintf "%s\n" (Unix.error_message err);
    Exit_status.(exit Socket_error)

(* The sockaddr_un structure puts a strict limit on the length of a socket
   address. This appears to be 104 chars on mac os x and 108 chars on my
   centos box. Since `with_addr` uses a relative path, `get_path` shortens
   the basename to fit if necessary. *)
let max_addr_length = 103

let get_path path =
  (* Path will resolve the realpath, in case two processes are referring to the
   * same socket using different paths (like with symlinks *)
  let path = path |> Path.make |> Path.to_string in
  let dir = Filename.dirname path in
  let filename = Filename.basename path in
  let root_part = Filename.chop_extension filename in
  let root_length = String.length root_part in
  let extension_length = String.length filename - root_length in
  let extension = String.sub filename root_length extension_length in
  let dir_sep_length = String.length Filename.dir_sep in
  let max_root_part_length = max_addr_length - dir_sep_length - extension_length - 1 in
  let root_part =
    if root_length > max_root_part_length then
      let len = String.length root_part in
      let prefix = String.sub root_part 0 5 in
      let suffix = String.sub root_part (len - 5) 5 in
      let digest = OpaqueDigest.to_hex (OpaqueDigest.string root_part) in
      (* 5 char prefix + 5 char suffix + 2 periods *)
      let max_digest_length = max_root_part_length - 12 in
      let digest_part =
        if max_digest_length <= 0 then
          let () = Printf.eprintf "Socket name is too long: %S\n" filename in
          raise Exit_status.(Exit_with Socket_error)
        else if String.length digest > max_digest_length then
          String.sub digest 0 max_digest_length
        else
          digest
      in
      prefix ^ "." ^ digest_part ^ "." ^ suffix
    else
      root_part
  in
  Filename.concat dir (Printf.sprintf "%s%s" root_part extension)

let addr_for_open sockfile =
  let sock_name = get_path sockfile in
  if Sys.win32 then (
    let ic = open_in_bin sock_name in
    let port = input_binary_int ic in
    close_in ic;
    Inet (Unix.inet_addr_loopback, port)
  ) else
    Unix sock_name

let init_unix_socket socket_file = unix_socket (get_path socket_file)
