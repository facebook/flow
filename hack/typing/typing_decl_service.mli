(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Module declaring the types in parallel *)
(*****************************************************************************)
open Utils

(* The set of files that failed *)
type failed = Relative_path.Set.t
(* The result expected from the service *)
type result = Errors.t * failed

(*****************************************************************************)
(* We need to know all the classes defined, because we want to declare
 * the types in their topological order *)
(*****************************************************************************)
val get_classes: FileInfo.fast -> Relative_path.Set.t SMap.t

(*****************************************************************************)
(* Starts the process *)
(*****************************************************************************)
val go: Worker.t list option -> bucket_size:int -> TypecheckerOptions.t ->
  FileInfo.fast -> result
val merge_decl: result -> result -> result
