(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

exception Invalid_Int_Size_Exception

exception Payload_Size_Too_Large_Exception

exception Malformed_Preamble_Exception

exception Writing_Preamble_Exception

exception Writing_Payload_Exception

exception Reading_Preamble_Exception

exception Reading_Payload_Exception

type remote_exception_data = {
  message: string;
  stack: string;
}

val to_fd_with_preamble :
  ?timeout:Timeout.t ->
  ?flags:Marshal.extern_flags list ->
  Unix.file_descr ->
  'a ->
  int

val from_fd_with_preamble : ?timeout:Timeout.t -> Unix.file_descr -> 'a

module type WRITER_READER = sig
  type 'a result

  type fd

  val return : 'a -> 'a result

  val fail : exn -> 'a result

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result

  val write :
    ?timeout:Timeout.t ->
    fd ->
    buffer:bytes ->
    offset:int ->
    size:int ->
    int result

  val read :
    ?timeout:Timeout.t ->
    fd ->
    buffer:bytes ->
    offset:int ->
    size:int ->
    int result

  val log : string -> unit
end

module MarshalToolsFunctor (WriterReader : WRITER_READER) : sig
  val expected_preamble_size : int

  val to_fd_with_preamble :
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    WriterReader.fd ->
    'a ->
    int WriterReader.result

  val from_fd_with_preamble :
    ?timeout:Timeout.t -> WriterReader.fd -> 'a WriterReader.result
end
