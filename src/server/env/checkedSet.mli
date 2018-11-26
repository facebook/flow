(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
val empty: t
val is_empty: t -> bool
val of_focused_list: File_key.t list -> t

val cardinal: t -> int

val mem: File_key.t -> t -> bool
val add:
  ?focused:Utils_js.FilenameSet.t ->
  ?dependents:Utils_js.FilenameSet.t ->
  ?dependencies:Utils_js.FilenameSet.t ->
  t ->
  t
val remove: Utils_js.FilenameSet.t -> t -> t

val fold: ('a -> File_key.t -> 'a) -> 'a -> t -> 'a

val union: t -> t -> t
val diff: t -> t -> t

val filter: f:(File_key.t -> bool) -> t -> t

val all: t -> Utils_js.FilenameSet.t
val focused: t -> Utils_js.FilenameSet.t
val dependents: t -> Utils_js.FilenameSet.t
val dependencies: t -> Utils_js.FilenameSet.t

val debug_to_string: ?limit:int -> t -> string
val debug_counts_to_string: t -> string
