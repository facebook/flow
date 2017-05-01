(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(**
 * This module is needed because Unix.select doesn't play well with
 * input_line on Ocaml channels.. i.e., when a buffered read into an
 * Ocaml channel consumes two complete lines from the file descriptor, the next
 * select will say there is nothing to read when in fact there is
 * something in the channel. This wouldn't be a problem if Ocaml channel's API
 * supported a "has buffered content" call, so you could check if the
 * buffer contains something as well as doing a Unix select to know for real if
 * there is content coming.
 *
 * The "has_buffered_content" method below does exactly that.
 *)

type t

val create: Unix.file_descr -> t

val get_null_reader: unit -> t

val has_buffered_content: t -> bool

val get_fd: t -> Unix.file_descr

val get_next_line: ?approx_size: int -> t -> string

val get_next_bytes: t -> int -> string
