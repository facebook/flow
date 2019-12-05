(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Disk_sig.Types

let cat (filename : string) : string =
  let ic = open_in_bin filename in
  let len = (try in_channel_length ic with Sys_error _ -> 0) in
  (* in_channel_length returns 0 for non-regular files; try reading it
    using a fixed-sized buffer if it appears to be empty.
    NOTE: JaneStreet's Core Sys module defines a function is_file which
    does a proper check on whether the file exists and is regular. *)
  if len > 0 then (
    let buf = Buffer.create len in
    Buffer.add_channel buf ic len;
    close_in ic;
    Buffer.contents buf
  ) else
    let len = 1024 in
    (* for Buffer, that's the initial size of the internal byte sequence *)
    let buf = Buffer.create len in
    let bytes = Bytes.create len in
    let rec read_bytes () : unit =
      try
        let n = input ic bytes 0 len in
        if n = 0 then
          ()
        else (
          Buffer.add_subbytes buf bytes 0 n;

          (* 0 is offset *)
          read_bytes ()
        )
      with End_of_file -> ()
    in
    read_bytes ();
    close_in ic;
    Buffer.contents buf

let is_file_not_exist_error ~file ~err_msg =
  let msg = Printf.sprintf "%s: No such file or directory" file in
  msg = err_msg

let write_file ~file ~contents =
  let chan =
    try open_out file
    with Sys_error err_msg when is_file_not_exist_error ~file ~err_msg ->
      raise (No_such_file_or_directory file)
  in
  output_string chan contents;
  close_out chan

let rec mkdir_p = function
  | "" -> failwith "Unexpected empty directory, should never happen"
  | d when not (Sys.file_exists d) ->
    mkdir_p (Filename.dirname d);
    Unix.mkdir d 0o777
  | d when Sys.is_directory d -> ()
  | d -> raise (NotADirectory d)

let rec rm_dir_tree path =
  try
    let stats = Unix.lstat path in
    match stats.Unix.st_kind with
    | Unix.S_DIR ->
      let contents = Sys.readdir path in
      List.iter
        (fun name ->
          let name = Filename.concat path name in
          rm_dir_tree name)
        (Array.to_list contents);
      Unix.rmdir path
    | Unix.S_LNK
    | Unix.S_REG
    | Unix.S_CHR
    | Unix.S_BLK
    | Unix.S_FIFO
    | Unix.S_SOCK ->
      Unix.unlink path
  with
  (* Path has been deleted out from under us - can ignore it. *)
  | Sys_error s when s = Printf.sprintf "%s: No such file or directory" path -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let is_directory x = (try Sys.is_directory x with Sys_error _ -> false)

let file_exists = Sys.file_exists

let getcwd = Sys.getcwd

let chdir = Sys.chdir

let mkdir = Unix.mkdir

let readdir = Sys.readdir

let rename old target =
  if not (file_exists old) then
    raise (No_such_file_or_directory old)
  else if not (file_exists (Filename.dirname target)) then
    raise (No_such_file_or_directory (Filename.dirname target))
  else
    try Sys.rename old target
    with Sys_error s when s = "Directory not empty" -> raise (Rename_target_dir_not_empty target)

let filemtime file = (Unix.stat file).Unix.st_mtime
