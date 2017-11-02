(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open Hh_core

(** Our Unix systems only allow reading 64KB chunks at a time.
 * Trying to read more than 64KB results in only 64KB being read. *)
let chunk_size = 65536

type t = {
  fd: Unix.file_descr;
  (** The bytes left after the last content that haven't been consumed yet. *)
  unconsumed_buffer: string option ref;
}

let set_buffer r b =
  r.unconsumed_buffer := b

(** A non-throwing version of String.index. *)
let index s c =
  try begin
    let i = String.index s c in
    `First_appearance i
  end
  with
  | Not_found -> `No_appearance

let trim_trailing_cr = function
  | "" -> ""
  | s ->
    let len = String.length s in
    if s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let merge_chunks last_chunk chunks_rev =
  let chunks_rev = last_chunk :: chunks_rev in
  let chunks = List.rev chunks_rev in
  String.concat "" chunks

(** This function reads a line, delimited by LF (unix) or CRLF (internet)...
 * Recursively read a chunk from the file descriptor until we see a newline
 * character, building up the chunks accumulator along the way.
 * Any remaining bytes not consumed (bytes past the newline) are placed in the
 * reader's unconsumed_buffer which will be consumed on the next
 * call to get_next_line or get_next_bytes. *)
let rec read_line chunks r =
  let b = Bytes.create chunk_size in

  let bytes_read = Unix.read r.fd b 0 chunk_size in
  if bytes_read == 0 then raise End_of_file;
  let b = String.sub b 0 bytes_read in

  match index b '\n' with
  | `No_appearance ->
    read_line (b :: chunks) r
  | `First_appearance i ->
    let tail = String.sub b 0 i in
    let result = merge_chunks tail chunks in
    let () = if (i + 1) < bytes_read
    then
      (** We read some bytes beyond the first newline character. *)
      let length = bytes_read - (i + 1) in
      (** We skip the newline character. *)
      let remainder = String.sub b (i + 1) length in
      set_buffer r (Some remainder)
    else
      (** We didn't read any bytes beyond the first newline character. *)
      set_buffer r None
    in
    trim_trailing_cr result

let get_next_line ?approx_size r =
  match !(r.unconsumed_buffer) with
  | None -> read_line [] r
  | Some remainder -> begin
    match index remainder '\n' with
    | `No_appearance ->
      let () = set_buffer r None in
      read_line [remainder] r
    | `First_appearance i ->
      let result = String.sub remainder 0 i in
      let () = if (i + 1) < (String.length remainder)
      then
        (** There are some bytes left beyond the first newline character. *)
        let length = (String.length remainder) - (i + 1) in
        let remainder = String.sub remainder (i + 1) length in
        set_buffer r (Some remainder)
      else
        (** No bytes beyond the first newline character. *)
        set_buffer r None
      in
      trim_trailing_cr result
  end

let rec read_bytes r size chunks =
  let bytes_desired = min chunk_size size in
  let b = Bytes.create bytes_desired in
  let bytes_read = Unix.read r.fd b 0 bytes_desired in
  if bytes_read == 0 then raise End_of_file;
  if bytes_read < size then
    let b = String.sub b 0 bytes_read in
    read_bytes r (size - bytes_read) (b :: chunks)
  else
    let () = set_buffer r None in
    merge_chunks b chunks

let get_next_bytes r size =
  assert (size > 0);
  match !(r.unconsumed_buffer) with
  | None -> read_bytes r size []
  | Some remainder -> begin
    let remainder_length = String.length remainder in
    if remainder_length < size then
      let () = set_buffer r None in
      read_bytes r (size - remainder_length) [remainder]
    else if remainder_length = size then
      let () = set_buffer r None in
      remainder
    else
      let extra = String.sub remainder size (remainder_length - size) in
      let () = set_buffer r (Some extra) in
      String.sub remainder 0 size
    end

let has_buffered_content r = !(r.unconsumed_buffer) <> None


let is_readable r =
  if has_buffered_content r then
    true
  else
    let readable, _, _ = Unix.select [r.fd] [] [] 0.0 in
    readable <> []

let create fd = {
  fd = fd;
  unconsumed_buffer = ref None;
  }

let null_reader_ref = ref None

let get_null_reader () =
  match !null_reader_ref with
    | Some x -> x
    | None ->
        let null_reader = {
          fd = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o440;
          unconsumed_buffer = ref None;
        } in
        null_reader_ref := Some null_reader;
        null_reader

let get_fd r = r.fd
