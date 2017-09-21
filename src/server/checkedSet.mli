(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t
val empty: t
val of_focused_list: Loc.filename list -> t

val mem: Loc.filename -> t -> bool
val add:
  ?focused:Utils_js.FilenameSet.t ->
  ?dependents:Utils_js.FilenameSet.t ->
  ?dependencies:Utils_js.FilenameSet.t ->
  t ->
  t
val remove: Utils_js.FilenameSet.t -> t -> t

val fold: ('a -> Loc.filename -> 'a) -> 'a -> t -> 'a

val union: t -> t -> t
val diff: t -> t -> t

val filter: f:(Loc.filename -> bool) -> t -> t

val all: t -> Utils_js.FilenameSet.t
val focused: t -> Utils_js.FilenameSet.t
val dependents: t -> Utils_js.FilenameSet.t
val dependencies: t -> Utils_js.FilenameSet.t

val debug_to_string: t -> string
val debug_counts_to_string: t -> string
