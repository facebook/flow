(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val is_php_path: string -> bool
val is_js_path: string -> bool

val make_next_files_with_find:
  (string -> bool) -> ?others: Path.t list -> Path.t ->
  string MultiWorker.nextlist

val make_next_files_php:
  ?others: Path.t list (* includes *) -> Path.t (* root directory *) ->
  string MultiWorker.nextlist
val make_next_files_js:
  filter:(string -> bool) ->
  ?others: Path.t list (* includes *) -> Path.t (* root directory *) ->
  string MultiWorker.nextlist

val find_with_name : Path.t list -> string -> string list
