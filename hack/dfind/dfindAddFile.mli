(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(*****************************************************************************)
(* Adds a new file or directory to the environment *)
(*****************************************************************************)

val path : DfindEnv.t -> string -> unit

(*****************************************************************************)
(* Find all the files in a directory *)
(*****************************************************************************)

val get_files : string -> Unix.dir_handle -> SSet.t
