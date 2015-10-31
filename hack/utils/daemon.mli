(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Type-safe versions of the channels in Pervasives. *)

type 'a in_channel
type 'a out_channel
type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel

val to_channel :
  'a out_channel -> ?flags:Marshal.extern_flags list -> ?flush:bool ->
  'a -> unit
val from_channel : ?timeout:Timeout.t -> 'a in_channel -> 'a
val flush : 'a out_channel -> unit

(* This breaks the type safety, but is necessary in order to allow select() *)
val descr_of_in_channel : 'a in_channel -> Unix.file_descr
val descr_of_out_channel : 'a out_channel -> Unix.file_descr

(** Spawning new process *)

(* In the absence of 'fork' on Windows, its usage must be restricted
   to Unix specifics parts.

   This module provides a mechanism to "spawn" new instance of the
   current program, but with a custom entry point (e.g. Slaves,
   DfindServer, ...). Then, alternate entry points should not depend
   on global references that may not have been (re)initialised in the
   new process.

   All required data must be passed through the typed channels.
   associated to the spawned process.

 *)

(* Alternate entry points *)
type ('param, 'input, 'output) entry

(* Alternate entry points must be registered at toplevel, i.e.
   every call to `Daemon.register_entry_point` must have been
   evaluated when `Daemon.check_entry_point` is called at the
   beginning of `ServerMain.start`. *)
val register_entry_point :
  string -> ('param -> ('input, 'output) channel_pair -> unit) ->
  ('param, 'input, 'output) entry

(* Handler upon spawn and forked process. *)
type ('in_, 'out) handle = {
  channels : ('in_, 'out) channel_pair;
  pid : int;
}

(* Fork and run a function that communicates via the typed channels *)
val fork : ?log_file:string -> (('input, 'output) channel_pair -> unit) ->
  ('output, 'input) handle

(* for unit tests *)
val devnull : unit -> ('a, 'b) handle

(* Spawn a new instance of the current process, and execute the
   alternate entry point. *)
val spawn :
  ?reason:string -> ?log_file:string ->
  ('param, 'input, 'output) entry -> 'param -> ('output, 'input) handle

(* Close the typed channels associated to a 'spawned' child. *)
val close : ('a, 'b) handle -> unit

(* Kill a 'spawned' child and close the associated typed channels. *)
val kill : ('a, 'b) handle -> unit

(* Main function, that execute a alternate entry point.
   It should be called only once. Just before the main entry point.
   This function does not return when a custom entry point is selected. *)
val check_entry_point : unit -> unit
