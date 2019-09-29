(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core

(* Our Unix systems only allow reading 64KB chunks at a time.
 * Trying to read more than 64KB results in only 64KB being read. *)
let chunk_size = 65536

module Regular_reader :
  Buffered_line_reader_sig.READER
    with type 'a result = 'a
     and type fd = Unix.file_descr = struct
  type 'a result = 'a

  type fd = Unix.file_descr

  let return x = x

  let fail exn = raise exn

  let ( >>= ) a f = f a

  let read fd ~buffer ~offset ~size = Unix.read fd buffer offset size

  let is_readable fd =
    let (readable, _, _) = Unix.select [fd] [] [] 0.0 in
    readable <> []

  let open_devnull () = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o440
end

module Functor (Reader : Buffered_line_reader_sig.READER) :
  Buffered_line_reader_sig.S
    with type 'a result = 'a Reader.result
     and type fd = Reader.fd = struct
  let ( >>= ) = Reader.( >>= )

  type fd = Reader.fd

  type 'a result = 'a Reader.result

  type t = {
    fd: Reader.fd;
    (* The bytes left after the last content that haven't been consumed yet. *)
    unconsumed_buffer: string option ref;
  }

  let set_buffer r b = r.unconsumed_buffer := b

  (** A non-throwing version of String.index. *)
  let index s c =
    try
      let i = String.index s c in
      `First_appearance i
    with Not_found -> `No_appearance

  let trim_trailing_cr = function
    | "" -> ""
    | s ->
      let len = String.length s in
      if s.[len - 1] = '\r' then
        String.sub s 0 (len - 1)
      else
        s

  let merge_chunks last_chunk chunks_rev =
    let chunks_rev = last_chunk :: chunks_rev in
    let chunks = List.rev chunks_rev in
    String.concat "" chunks

  (* This function reads a line, delimited by LF (unix) or CRLF (internet)...
   * Recursively read a chunk from the file descriptor until we see a newline
   * character, building up the chunks accumulator along the way.
   * Any remaining bytes not consumed (bytes past the newline) are placed in the
   * reader's unconsumed_buffer which will be consumed on the next
   * call to get_next_line or get_next_bytes. *)
  let rec read_line chunks r =
    let b = Bytes.create chunk_size in
    Reader.read r.fd ~buffer:b ~offset:0 ~size:chunk_size
    >>= fun bytes_read ->
    if bytes_read == 0 then raise End_of_file;
    let b = Bytes.sub_string b 0 bytes_read in
    match index b '\n' with
    | `No_appearance -> read_line (b :: chunks) r
    | `First_appearance i ->
      let tail = String.sub b 0 i in
      let result = merge_chunks tail chunks in
      let () =
        if i + 1 < bytes_read then
          (* We read some bytes beyond the first newline character. *)
          let length = bytes_read - (i + 1) in
          (* We skip the newline character. *)
          let remainder = String.sub b (i + 1) length in
          set_buffer r (Some remainder)
        else
          (* We didn't read any bytes beyond the first newline character. *)
          set_buffer r None
      in
      Reader.return @@ trim_trailing_cr result

  let get_next_line r =
    match !(r.unconsumed_buffer) with
    | None -> read_line [] r
    | Some remainder ->
      begin
        match index remainder '\n' with
        | `No_appearance ->
          let () = set_buffer r None in
          read_line [remainder] r
        | `First_appearance i ->
          let result = String.sub remainder 0 i in
          let () =
            if i + 1 < String.length remainder then
              (* There are some bytes left beyond the first newline character. *)
              let length = String.length remainder - (i + 1) in
              let remainder = String.sub remainder (i + 1) length in
              set_buffer r (Some remainder)
            else
              (* No bytes beyond the first newline character. *)
              set_buffer r None
          in
          Reader.return @@ trim_trailing_cr result
      end

  let rec read_bytes r size chunks =
    let bytes_desired = min chunk_size size in
    let b = Bytes.create bytes_desired in
    Reader.read r.fd ~buffer:b ~offset:0 ~size:bytes_desired
    >>= fun bytes_read ->
    if bytes_read == 0 then raise End_of_file;
    if bytes_read < size then
      let b = Bytes.sub_string b 0 bytes_read in
      read_bytes r (size - bytes_read) (b :: chunks)
    else
      let () = set_buffer r None in
      (* `unsafe_to_string` is acceptable here because `merge_chunks`
         immediately makes a copy via `concat` *)
      Reader.return @@ merge_chunks (Bytes.unsafe_to_string b) chunks

  let get_next_bytes r size =
    assert (size > 0);
    match !(r.unconsumed_buffer) with
    | None -> read_bytes r size []
    | Some remainder ->
      let remainder_length = String.length remainder in
      if remainder_length < size then
        let () = set_buffer r None in
        read_bytes r (size - remainder_length) [remainder]
      else if remainder_length = size then
        let () = set_buffer r None in
        Reader.return @@ remainder
      else
        let extra = String.sub remainder size (remainder_length - size) in
        let () = set_buffer r (Some extra) in
        Reader.return @@ String.sub remainder 0 size

  let has_buffered_content r = !(r.unconsumed_buffer) <> None

  let is_readable r = has_buffered_content r || Reader.is_readable r.fd

  let create fd = { fd; unconsumed_buffer = ref None }

  let null_reader_ref = ref None

  let get_null_reader () =
    match !null_reader_ref with
    | Some x -> Reader.return x
    | None ->
      Reader.open_devnull ()
      >>= fun fd ->
      let null_reader = { fd; unconsumed_buffer = ref None } in
      null_reader_ref := Some null_reader;
      Reader.return null_reader

  let get_fd r = r.fd
end

include Functor (Regular_reader)
