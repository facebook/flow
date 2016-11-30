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
 * The "has_pending_line" method below does exactly that.
 *)

open Core

type t = {
  fd: Unix.file_descr;
  pending_whole_lines : string Queue.t;
}

let rec read_message ?line_part:(line_part="") r =
  let b = String.create 1024 in

  let bytes_read = Unix.read r.fd b 0 1024 in
  assert (bytes_read > 0);

  let last_line_complete = String.get b (bytes_read - 1) = '\n' in
  let s = line_part ^ (String.sub b 0 bytes_read) in

  let lines = Str.split (Str.regexp "\n") s in
  let lines_read = List.length lines in

  let last_line_part = ref "" in

  List.iteri lines ~f:begin fun i line ->
    if last_line_complete || i < lines_read - 1 then
      Queue.push line r.pending_whole_lines
    else last_line_part := line
  end;

  if last_line_complete
    then Queue.take r.pending_whole_lines
    else read_message ~line_part:!last_line_part r

let get_next_line r =
  if Queue.is_empty r.pending_whole_lines
    then read_message r
    else Queue.take r.pending_whole_lines

let has_pending_line r =
  not @@ Queue.is_empty r.pending_whole_lines

let create fd = {
  fd = fd;
  pending_whole_lines = Queue.create ();
  }

let get_fd r = r.fd
