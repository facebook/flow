(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This tool allows for marshaling directly over file descriptors (instead of
 * ocaml "channels") to avoid buffering so that we can safely use marshaling
 * and libancillary together.
 *
 * The problem:
   * Ocaml's marshaling is done over channels, which have their own internal
   * buffer. This means after reading a marshaled object from a channel, the
   * FD's position is not guaranteed to be pointing to the beginning of the
   * next marshaled object (but instead points to the position after the
   * buffered read). So another process cannot receive this FD (over
   * libancillary) to start reading the next object.
   *
 * The solution:
   * Use unbuffered IO directly with Unix file descriptors. We can read the size
   * of the serialized value by first reading the header then extracting the
   * size, using the Marshal.data_size API.
 *)

exception Writing_Payload_Exception

(* We want to marshal exceptions (or at least their message+stacktrace) over  *)
(* the wire. This type ensures that no one will attempt to pattern-match on   *)
(* the thing we marshal: 'Values of extensible variant types, for example     *)
(* exceptions (of extensible type exn), returned by the unmarhsaller should   *)
(* not be pattern-matched over, because unmarshalling does not preserve the   *)
(* information required for matching their constructors.'                     *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html            *)
type remote_exception_data = {
  message: string;
  stack: string;
}

module type WRITER_READER = sig
  type 'a result

  type fd

  val return : 'a -> 'a result

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result

  val write : ?timeout:Timeout.t -> fd -> buffer:bytes -> offset:int -> size:int -> int result

  val read : ?timeout:Timeout.t -> fd -> buffer:bytes -> offset:int -> size:int -> int result
end

module type REGULAR_WRITER_READER =
  WRITER_READER with type 'a result = 'a and type fd = Unix.file_descr

module RegularWriterReader : REGULAR_WRITER_READER = struct
  type 'a result = 'a

  type fd = Unix.file_descr

  let return x = x

  let ( >>= ) a f = f a

  let rec write ?timeout fd ~buffer ~offset ~size =
    match Timeout.select ?timeout [] [fd] [] ~-.1.0 with
    | (_, [], _) -> 0
    | _ ->
      (* Timeout.select handles EINTR, but the Unix.write call can also be interrupted. If the write
       * is interrupted before any bytes are written, the call fails with EINTR. Otherwise, the call
       * succeeds and returns the number of bytes written.
       *)
      (try Unix.write fd buffer offset size with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write ?timeout fd ~buffer ~offset ~size)

  (* Marshal_tools reads from file descriptors. These file descriptors might be for some
   * non-blocking socket. Normally if you try to read from an fd, it will block until some data is
   * ready. But if you try to read from a non-blocking socket and it's not ready, you get an
   * EWOULDBLOCK error.
   *
   * People using Marshal_tools probably are calling Unix.select first. However that only guarantees
   * that the first read won't block. Marshal_tools will always do at least 2 reads (one for the
   * header and one or more for the data). Any read after the first might block.
   *)
  let rec read ?timeout fd ~buffer ~offset ~size =
    match Timeout.select ?timeout [fd] [] [] ~-.1.0 with
    | ([], _, _) -> 0
    | _ ->
      (* Timeout.select handles EINTR, but the Unix.read call can also be interrupted. If the read
       * is interrupted before any bytes are read, the call fails with EINTR. Otherwise, the call
       * succeeds and returns the number of bytes read.
       *)
      (try Unix.read fd buffer offset size with
      | Unix.Unix_error (Unix.EINTR, _, _) -> read ?timeout fd ~buffer ~offset ~size)
end

module MarshalToolsFunctor (WriterReader : WRITER_READER) : sig
  val to_fd :
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    WriterReader.fd ->
    'a ->
    int WriterReader.result

  val from_fd : ?timeout:Timeout.t -> WriterReader.fd -> 'a WriterReader.result
end = struct
  let ( >>= ) = WriterReader.( >>= )

  let rec write_payload ?timeout fd buffer offset to_write =
    if to_write = 0 then
      WriterReader.return offset
    else
      WriterReader.write ?timeout fd ~buffer ~offset ~size:to_write >>= fun bytes_written ->
      if bytes_written = 0 then
        WriterReader.return offset
      else
        write_payload ?timeout fd buffer (offset + bytes_written) (to_write - bytes_written)

  (* Returns the size of the marshaled payload *)
  let to_fd ?timeout ?(flags = []) fd obj =
    let payload = Marshal.to_bytes obj flags in
    let size = Bytes.length payload in
    write_payload ?timeout fd payload 0 size >>= fun bytes_written ->
    if bytes_written <> size then
      raise Writing_Payload_Exception
    else
      WriterReader.return size

  let rec read_payload ?timeout fd buffer offset to_read =
    if to_read = 0 then
      WriterReader.return offset
    else
      WriterReader.read ?timeout fd ~buffer ~offset ~size:to_read >>= fun bytes_read ->
      if bytes_read = 0 then
        WriterReader.return offset
      else
        read_payload ?timeout fd buffer (offset + bytes_read) (to_read - bytes_read)

  let from_fd ?timeout fd =
    let header = Bytes.create Marshal.header_size in
    read_payload ?timeout fd header 0 Marshal.header_size >>= fun bytes_read ->
    if bytes_read <> Marshal.header_size then
      raise End_of_file
    else
      let data_size = Marshal.data_size header 0 in
      let payload = Bytes.create (Marshal.header_size + data_size) in
      Bytes.unsafe_blit header 0 payload 0 Marshal.header_size;
      read_payload ?timeout fd payload Marshal.header_size data_size >>= fun offset_after_read ->
      let data_size_read = offset_after_read - Marshal.header_size in
      if data_size_read <> data_size then
        raise End_of_file
      else
        WriterReader.return (Marshal.from_bytes payload 0)
end

module RegularMarshalTools = MarshalToolsFunctor (RegularWriterReader)
include RegularMarshalTools
