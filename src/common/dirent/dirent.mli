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

val compare : t -> t -> int

val opendir : string -> Unix.dir_handle

val readdir : Unix.dir_handle -> t

val closedir : Unix.dir_handle -> unit

val entries : string -> t array
