(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type file_kind =
| Dir of string
| File of string

val fold_files:
  ?max_depth:int -> ?filter:(string -> bool) -> ?file_only:bool ->
  string list ->
  (file_kind -> 'a -> 'a) ->
  'a ->
  'a
