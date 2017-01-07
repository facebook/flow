(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Uttilities to deal with subprocesses. *)

(** exec program ?env args
 *
 * Shells out the program with the given args. *)
val exec : string -> ?env:string list -> string list -> Process_types.t

val read_and_close_pid : Process_types.t -> string
