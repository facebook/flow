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
  (string -> bool) -> ?others: Path.path list -> Path.path ->
  string MultiWorker.nextlist

val make_next_files_php:
  ?others: Path.path list (* includes *) -> Path.path (* root directory *) ->
  string MultiWorker.nextlist
val make_next_files_js:
  filter:(string -> bool) ->
  ?others: Path.path list (* includes *) -> Path.path (* root directory *) ->
  string MultiWorker.nextlist

val find_with_name : Path.path list -> string -> string list
