(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type d_type =
  | DT_UNKNOWN
  | DT_FIFO
  | DT_CHR
  | DT_DIR
  | DT_BLK
  | DT_REG
  | DT_LNK
  | DT_SOCK

type t = {
  d_name: string;
  d_type: d_type;
}

let compare { d_name = a; _ } { d_name = b; _ } = String.compare a b

let opendir = Unix.opendir

let closedir = Unix.closedir

external readdir_unix : Unix.dir_handle -> t = "flow_dirent_readdir"

let readdir_win32 handle =
  let d_name = Unix.readdir handle in
  { d_name; d_type = DT_UNKNOWN }

let readdir =
  if Sys.win32 then
    readdir_win32
  else
    readdir_unix

external entries : string -> t array = "flow_dirent_entries"
