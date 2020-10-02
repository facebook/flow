(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_kind =
  | Dir of string
  | File of string

val fold_files :
  ?max_depth:int ->
  ?filter:(string -> bool) ->
  ?file_only:bool ->
  string list ->
  (file_kind -> 'a -> 'a) ->
  'a ->
  'a
