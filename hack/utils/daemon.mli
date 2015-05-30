(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Type-safe versions of the channels in Pervasives. *)
type 'a in_channel
type 'a out_channel
type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel
type ('in_, 'out) handle = {
  channels : ('in_, 'out) channel_pair;
  pid : int;
}

val to_channel : 'a out_channel -> ?flush:bool -> 'a -> unit
val from_channel : 'a in_channel -> 'a
val flush : 'a out_channel -> unit
(* This breaks the type safety, but is necessary in order to allow select() *)
val descr_of_in_channel : 'a in_channel -> Unix.file_descr
val descr_of_out_channel : 'a out_channel -> Unix.file_descr

(* Fork and run a function that communicates via the typed channels *)
val fork : ?log_name:string -> (('a, 'b) channel_pair -> unit) ->
  ('b, 'a) handle

(* for unit tests *)
val devnull : unit -> ('a, 'b) handle
