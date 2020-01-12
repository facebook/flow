(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type CONNECTION_PROCESSOR = sig
  type in_message

  type out_message
end

module type CONNECTION = sig
  type t

  type in_message

  type out_message

  val create :
    name:string ->
    in_fd:Lwt_unix.file_descr ->
    out_fd:Lwt_unix.file_descr ->
    close:(unit -> unit Lwt.t) ->
    on_read:(msg:in_message -> connection:t -> unit Lwt.t) ->
    ((unit -> unit) * t) Lwt.t

  val write : msg:out_message -> t -> unit

  val write_and_close : msg:out_message -> t -> unit

  val close_immediately : t -> unit Lwt.t

  val flush_and_close : t -> unit Lwt.t

  val is_closed : t -> bool

  val wait_for_closed : t -> unit Lwt.t
end

module Make (ConnectionProcessor : CONNECTION_PROCESSOR) :
  CONNECTION
    with type in_message := ConnectionProcessor.in_message
     and type out_message := ConnectionProcessor.out_message
