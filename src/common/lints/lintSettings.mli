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

val kinds_of_string: string -> lint_kind list

type t

val default_settings: t

val all_setting: bool -> t

val set_enabled: lint_kind -> bool -> t -> t

val get_default: t -> bool

val is_enabled: lint_kind -> t -> bool
(* Always the logical opposite of is_enabled *)
val is_suppressed: lint_kind -> t -> bool
(* Iterate over all lint kinds with an explicit setting *)
val iter: (lint_kind -> bool -> unit) -> t -> unit
(* Merge two LintSettings, with rules in higher_precedence overwriting
 * rules in lower_precedencse. *)
val merge: low_prec:t -> high_prec:t -> t

val of_lines: ('a * string) list -> (t, 'a * string) result
