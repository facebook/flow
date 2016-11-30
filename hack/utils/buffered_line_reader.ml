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

open Core

(** Our Unix systems only allow reading 64KB chunks at a time.
 * Trying to read more than 64KB results in only 64KB being read. *)
let chunk_size = 65536

type t = {
  fd: Unix.file_descr;
  (** read_message is recursively called, prepending chunks to this reversed
   * list until a newline is found. This means only the first element in this
   * list may contain a (or more than one) newline (but doesn't necessarily).
   *)
  buffered_chunks_rev: string list ref;
}

let set_buffer_rev r b =
  r.buffered_chunks_rev := b

(** A non-throwing version of String.index. *)
let index s c =
  try begin
    let i = String.index s c in
    `First_appearance i
  end
  with
  | Not_found -> `No_appearance

let merge_chunks last_chunk chunks_rev =
  let chunks_rev = last_chunk :: chunks_rev in
  let chunks = List.rev chunks_rev in
  String.concat "" chunks

(** Recursively read a chunk from the file descriptor until we see a newline
 * character. This will build up the buffer if no newlines are read and will
 * consume the entirety of the buffer when a newline is encountered (resetting
 * the buffer with the remainder of the read chunk beyond that first newline,
 * which will be consumed on the next read_message call.).*)
let rec read_message r =
  let b = String.create chunk_size in

  let bytes_read = Unix.read r.fd b 0 chunk_size in
  assert (bytes_read > 0);
  let b = String.sub b 0 bytes_read in

  match index b '\n' with
  | `No_appearance ->
    let () = r.buffered_chunks_rev := b :: !(r.buffered_chunks_rev) in
    read_message r
  | `First_appearance i ->
    let tail = String.sub b 0 i in
    let result = merge_chunks tail !(r.buffered_chunks_rev) in
    let () = if (i + 1) < bytes_read
    then
      (** We read some bytes beyond the first newline character. *)
      let length = bytes_read - (i + 1) in
      (** We skip the newline character. *)
      let remainder = String.sub b (i + 1) length in
      set_buffer_rev r [remainder]
    else
      (** We didn't read any bytes beyond the first newline character. *)
      set_buffer_rev r []
    in
    result

let get_next_line ?approx_size r =
  match List.hd !(r.buffered_chunks_rev) with
  | None -> read_message r
  | Some remainder -> begin
    match index remainder '\n' with
    | `No_appearance ->
      read_message r
    | `First_appearance i ->
      let result = String.sub remainder 0 i in
      let () = if (i + 1) < (String.length remainder)
      then
        (** There are some bytes left beyond the first newline character. *)
        let length = (String.length remainder) - (i + 1) in
        let remainder = String.sub remainder (i + 1) length in
        set_buffer_rev r [remainder]
      else
        (** No bytes beyond the firstt newline character. *)
        set_buffer_rev r []
      in
      result
  end

let has_buffered_content r =
  not @@ List.is_empty !(r.buffered_chunks_rev)

let create fd = {
  fd = fd;
  buffered_chunks_rev = ref [];
  }

let null_reader = {
  fd = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o440;
  buffered_chunks_rev = ref [];
  }

let get_fd r = r.fd
