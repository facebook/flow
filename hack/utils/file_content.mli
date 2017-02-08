(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_api_types

val edit_file : string -> text_edit list -> (string, string) Result.t

val edit_file_unsafe : string -> text_edit list -> string

val get_offsets :
  string -> position * position -> int * int
