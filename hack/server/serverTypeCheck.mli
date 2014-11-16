(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val type_check: ServerEnv.genv -> ServerEnv.env -> ServerEnv.env

(* just add also some debugging information on stdout *)
val check: ServerEnv.genv -> ServerEnv.env -> ServerEnv.env

val hook_after_parsing: (ServerEnv.genv -> ServerEnv.env -> unit) ref
