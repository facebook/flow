(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val init :
  (Unix.file_descr * Unix.file_descr * Unix.file_descr) ->
  (string * Path.t list)
  -> t
val wait_until_ready : t -> unit
val pid : t -> int
val get_changes : ?timeout:Timeout.t -> t -> SSet.t

module type MARSHAL_TOOLS = sig
  type 'a result
  type fd

  val return: 'a -> 'a result
  val (>>=): 'a result -> ('a -> 'b result) -> 'b result

  val descr_of_in_channel: 'a Daemon.in_channel -> fd
  val descr_of_out_channel: 'a Daemon.out_channel -> fd

  val to_fd_with_preamble:
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    fd ->
    'a ->
    int result
  val from_fd_with_preamble: ?timeout:Timeout.t -> fd -> 'a result
end

module DFindLibFunctor :  functor (Marshal_tools: MARSHAL_TOOLS) -> sig
  type t
  val init :
    (Unix.file_descr * Unix.file_descr * Unix.file_descr) ->
    (string * Path.t list)
    -> t
  val wait_until_ready : t -> unit Marshal_tools.result
  val pid : t -> int
  val get_changes : ?timeout:Timeout.t -> t -> SSet.t Marshal_tools.result
end
