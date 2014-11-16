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
(* Adds a new file or directory to the environment *)
(*****************************************************************************)

val path: DfindEnv.t -> string -> unit

(*****************************************************************************)
(* Find all the files in a directory *)
(*****************************************************************************)

val get_files: string -> Unix.dir_handle -> DfindEnv.SSet.t
