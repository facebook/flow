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
  message : string;
  stack : string;
}

let preamble_start_sentinel = '\142'
(** Size in bytes. *)
let preamble_core_size = 4
let expected_preamble_size = preamble_core_size + 1
(** Payload size in bytes = 2^31 - 1. *)
let maximum_payload_size = (1 lsl (preamble_core_size * 8)) - 1

let get_preamble_core (size : int) =
  (** We limit payload size to 2^31 - 1 bytes. *)
  if size >= maximum_payload_size then
    raise Payload_Size_Too_Large_Exception;
  let rec loop i (remainder: int) acc =
    if i < 0 then acc
    else loop (i - 1) (remainder / 256)
      (String.set acc i (Char.chr (remainder mod 256)); acc) in
  loop (preamble_core_size - 1) size (String.create preamble_core_size)

let make_preamble (size : int) =
  let preamble_core = get_preamble_core size in
  let preamble = String.create (preamble_core_size + 1) in
  String.set preamble 0 preamble_start_sentinel;
  String.blit preamble_core 0 preamble 1 4;
  preamble

let parse_preamble preamble =
  if (String.length preamble) <> expected_preamble_size
    || (String.get preamble 0) <> preamble_start_sentinel then
    raise Malformed_Preamble_Exception;
  let rec loop i acc =
    if i >= 5 then acc
    else loop (i + 1) ((acc * 256) + (int_of_char (String.get preamble i))) in
  loop 1 0

let to_fd_with_preamble fd obj =
  let flag_list = [] in
  let payload = Marshal.to_string obj flag_list in
  let size = String.length payload in
  let preamble = make_preamble size in
  let preamble_bytes_written =
    Unix.write fd preamble 0 expected_preamble_size in
  if preamble_bytes_written <> expected_preamble_size then
    raise Writing_Preamble_Exception;
  let bytes_written = Unix.write fd payload 0 size in
  if bytes_written <> size then
    raise Writing_Payload_Exception;
  ()

(* Marshal_tools reads from file descriptors. These file descriptors might be for some
 * non-blocking socket. Normally if you try to read from an fd, it will block until some data is
 * ready. But if you try to read from a non-blocking socket and it's not ready, you get an
 * EWOULDBLOCK error.
 *
 * People using Marshal_tools probably are calling Unix.select first. However, that only guarantees
 * that the first read won't block. Marshal_tools will always do at least 2 reads (one for the
 * preamble and one or more for the data). Any read after the first might block.
 *
 * One day we should probably add a timeout to Marshal_tools, rather than blocking forever on
 * partially written data
 *)
let block_and_read fd buffer offset to_read =
  match Unix.select [fd] [] [] ~-.1.0 with
  | [], _, _ -> 0
  | _ -> Unix.read fd buffer offset to_read

let rec read_payload fd buffer offset to_read =
  if to_read = 0 then offset else begin
    let bytes_read = block_and_read fd buffer offset to_read in
    if bytes_read = 0 then offset else begin
      read_payload fd buffer (offset+bytes_read) (to_read-bytes_read)
    end
  end

let from_fd_with_preamble fd =
  let preamble = String.create expected_preamble_size in
  let bytes_read = block_and_read fd preamble 0 expected_preamble_size in
  if (bytes_read = 0)
  (** Unix manpage for read says 0 bytes read indicates end of file. *)
  then raise End_of_file
  else if (bytes_read <> expected_preamble_size) then
    (Printf.eprintf "Error, only read %d bytes for preamble.\n" bytes_read;
    raise Reading_Preamble_Exception);
  let payload_size = parse_preamble preamble in
  let payload = String.create payload_size in
  let payload_size_read = read_payload fd payload 0 payload_size in
  if (payload_size_read <> payload_size) then
    raise Reading_Payload_Exception;
  Marshal.from_string payload 0
