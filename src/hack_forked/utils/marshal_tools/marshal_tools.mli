(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

(** Some say we should represent network communications failures with results,
not exceptions. Here we go for those who favor results... *)
type error =
  | Rpc_absent of Exception.t  (** socket isn't open to start with *)
  | Rpc_disconnected of Exception.t  (** closed while trying to read/write *)
  | Rpc_malformed of string * Utils.callstack  (** malformed packet *)
  | Rpc_remote_panic of remote_exception_data  (** other party's unhandled exception *)

val error_to_verbose_string : error -> string
(** Turns an rpc_error into a detailed string suitable for debugging, maybe including stack trace *)

val to_fd_with_preamble :
  ?timeout:Timeout.t -> ?flags:Marshal.extern_flags list -> Unix.file_descr -> 'a -> int

val from_fd_with_preamble : ?timeout:Timeout.t -> Unix.file_descr -> 'a

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

module MarshalToolsFunctor (WriterReader : WRITER_READER) : sig
  val expected_preamble_size : int

  val to_fd_with_preamble :
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    WriterReader.fd ->
    'a ->
    int WriterReader.result

  val from_fd_with_preamble : ?timeout:Timeout.t -> WriterReader.fd -> 'a WriterReader.result
end
