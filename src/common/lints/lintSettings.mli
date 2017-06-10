(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type lint_kind =
  | SketchyNullBool
  | SketchyNullString
  | SketchyNullNumber
  | SketchyNullMixed

val string_of_kind: lint_kind -> string

type t

val default_settings: t

val fresh_settings: bool -> t

val set_enabled: lint_kind -> bool -> t -> t

val get_default: t -> bool

val is_suppressed: lint_kind -> t -> bool
(* Iterates over all lint kinds that are not implicitly set by the default *)
val iter: (lint_kind -> bool -> unit) -> t -> unit

val string_to_lints: string -> lint_kind list
