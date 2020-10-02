(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
   * Start each message with a fixed-size preamble that describes the
   * size of the payload to read. Read precisely that many bytes directly
   * from the FD avoiding Ocaml channels entirely.
 *)

exception Invalid_Int_Size_Exception

exception Payload_Size_Too_Large_Exception

exception Malformed_Preamble_Exception

exception Writing_Preamble_Exception

exception Writing_Payload_Exception

exception Reading_Preamble_Exception

exception Reading_Payload_Exception

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

type error =
  | Rpc_absent of Exception.t
  | Rpc_disconnected of Exception.t
  | Rpc_malformed of string * Utils.callstack
  | Rpc_remote_panic of remote_exception_data

let error_to_verbose_string (err : error) : string =
  match err with
  | Rpc_absent e -> "Absent: " ^ Exception.to_string e
  | Rpc_disconnected e -> "Disconnected: " ^ Exception.to_string e
  | Rpc_malformed (s, Utils.Callstack stack) -> Printf.sprintf "Malformed: %s\n%s" s stack
  | Rpc_remote_panic { message; stack } -> Printf.sprintf "Remote panic: %s\n%s" message stack

module type WRITER_READER = sig
  type 'a result

  type fd

  val return : 'a -> 'a result

  val fail : exn -> 'a result

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result

  val write : ?timeout:Timeout.t -> fd -> buffer:bytes -> offset:int -> size:int -> int result

  val read : ?timeout:Timeout.t -> fd -> buffer:bytes -> offset:int -> size:int -> int result

  val log : string -> unit
end

module type REGULAR_WRITER_READER =
  WRITER_READER with type 'a result = 'a and type fd = Unix.file_descr

module RegularWriterReader : REGULAR_WRITER_READER = struct
  type 'a result = 'a

  type fd = Unix.file_descr

  let return x = x

  let fail exn = raise exn

  let ( >>= ) a f = f a

  let rec write ?timeout fd ~buffer ~offset ~size =
    match Timeout.select ?timeout [] [fd] [] ~-.1.0 with
    | (_, [], _) -> 0
    | _ ->
      (* Timeout.select handles EINTR, but the Unix.write call can also be interrupted. If the write
       * is interrupted before any bytes are written, the call fails with EINTR. Otherwise, the call
       * succeeds and returns the number of bytes written.
       *)
      (try Unix.write fd buffer offset size
       with Unix.Unix_error (Unix.EINTR, _, _) -> write ?timeout fd ~buffer ~offset ~size)

  (* Marshal_tools reads from file descriptors. These file descriptors might be for some
   * non-blocking socket. Normally if you try to read from an fd, it will block until some data is
   * ready. But if you try to read from a non-blocking socket and it's not ready, you get an
   * EWOULDBLOCK error.
   *
   * People using Marshal_tools probably are calling Unix.select first. However that only guarantees
   * that the first read won't block. Marshal_tools will always do at least 2 reads (one for the
   * preamble and one or more for the data). Any read after the first might block.
   *)
  let rec read ?timeout fd ~buffer ~offset ~size =
    match Timeout.select ?timeout [fd] [] [] ~-.1.0 with
    | ([], _, _) -> 0
    | _ ->
      (* Timeout.select handles EINTR, but the Unix.read call can also be interrupted. If the read
       * is interrupted before any bytes are read, the call fails with EINTR. Otherwise, the call
       * succeeds and returns the number of bytes read.
       *)
      (try Unix.read fd buffer offset size
       with Unix.Unix_error (Unix.EINTR, _, _) -> read ?timeout fd ~buffer ~offset ~size)

  let log str = Printf.eprintf "%s\n%!" str
end

module MarshalToolsFunctor (WriterReader : WRITER_READER) : sig
  val expected_preamble_size : int

  val to_fd_with_preamble :
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    WriterReader.fd ->
    'a ->
    int WriterReader.result

  val from_fd_with_preamble : ?timeout:Timeout.t -> WriterReader.fd -> 'a WriterReader.result
end = struct
  let ( >>= ) = WriterReader.( >>= )

  let preamble_start_sentinel = '\142'

  (* Size in bytes. *)
  let preamble_core_size = 4

  let expected_preamble_size = preamble_core_size + 1

  (* Payload size in bytes = 2^31 - 1. *)
  let maximum_payload_size = (1 lsl (preamble_core_size * 8)) - 1

  let get_preamble_core (size : int) =
    (* We limit payload size to 2^31 - 1 bytes. *)
    if size >= maximum_payload_size then raise Payload_Size_Too_Large_Exception;
    let rec loop i (remainder : int) acc =
      if i < 0 then
        acc
      else
        loop
          (i - 1)
          (remainder / 256)
          ( Bytes.set acc i (Char.chr (remainder mod 256));
            acc )
    in
    loop (preamble_core_size - 1) size (Bytes.create preamble_core_size)

  let make_preamble (size : int) =
    let preamble_core = get_preamble_core size in
    let preamble = Bytes.create (preamble_core_size + 1) in
    Bytes.set preamble 0 preamble_start_sentinel;
    Bytes.blit preamble_core 0 preamble 1 4;
    preamble

  let parse_preamble preamble =
    if
      Bytes.length preamble <> expected_preamble_size
      || Bytes.get preamble 0 <> preamble_start_sentinel
    then
      raise Malformed_Preamble_Exception;
    let rec loop i acc =
      if i >= 5 then
        acc
      else
        loop (i + 1) ((acc * 256) + int_of_char (Bytes.get preamble i))
    in
    loop 1 0

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
  let to_fd_with_preamble ?timeout ?(flags = []) fd obj =
    let payload = Marshal.to_bytes obj flags in
    let size = Bytes.length payload in
    let preamble = make_preamble size in
    ( ( write_payload ?timeout fd preamble 0 expected_preamble_size >>= fun preamble_bytes_written ->
        if preamble_bytes_written <> expected_preamble_size then
          WriterReader.fail Writing_Preamble_Exception
        else
          WriterReader.return () )
    >>= fun () -> write_payload ?timeout fd payload 0 size )
    >>= fun bytes_written ->
    if bytes_written <> size then
      WriterReader.fail Writing_Payload_Exception
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

  let from_fd_with_preamble ?timeout fd =
    let preamble = Bytes.create expected_preamble_size in
    ( WriterReader.read ?timeout fd ~buffer:preamble ~offset:0 ~size:expected_preamble_size
    >>= fun bytes_read ->
      if bytes_read = 0 (* Unix manpage for read says 0 bytes read indicates end of file. *) then
        WriterReader.fail End_of_file
      else if bytes_read <> expected_preamble_size then (
        WriterReader.log (Printf.sprintf "Error, only read %d bytes for preamble." bytes_read);
        WriterReader.fail Reading_Preamble_Exception
      ) else
        WriterReader.return () )
    >>= fun () ->
    let payload_size = parse_preamble preamble in
    let payload = Bytes.create payload_size in
    read_payload ?timeout fd payload 0 payload_size >>= fun payload_size_read ->
    if payload_size_read <> payload_size then
      WriterReader.fail Reading_Payload_Exception
    else
      WriterReader.return (Marshal.from_bytes payload 0)
end

module RegularMarshalTools = MarshalToolsFunctor (RegularWriterReader)
include RegularMarshalTools
