(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Module used to save the state of the server. *)
(*****************************************************************************)

type dump = ServerEnv.env * ServerArgs.options

val dump_state: ServerEnv.env -> ServerEnv.genv -> dump

val save: dump -> string -> string
