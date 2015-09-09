(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
val empty_file_info: FileInfo.t

val go:
  Worker.t list option ->
  get_next:(unit -> Relative_path.t list) ->
  FileInfo.t Relative_path.Map.t * Errors.t * Relative_path.Set.t

(* used by hack build *)
val legacy_php_file_info: (Relative_path.t -> FileInfo.t) ref
