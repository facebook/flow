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
  | UntypedTypeImport

val string_of_kind: lint_kind -> string

val kinds_of_string: string -> lint_kind list option

type lint_state =
  | Off
  | Warn
  | Err

val string_of_state: lint_state -> string
val output_string_of_state: lint_state -> string

val state_of_string: string -> lint_state option

val state_cmp: lint_state -> lint_state -> int
val state_min: lint_state -> lint_state -> lint_state
val state_max: lint_state -> lint_state -> lint_state

type t

val default_settings: t

val all_setting: lint_state -> t

val set_state: lint_kind -> (lint_state * Loc.t option) -> t -> t

val set_all: (lint_kind * (lint_state * Loc.t option)) list -> t -> t

val get_default: t -> lint_state
(* Get the state of a lint kind in the provided settings *)
val get_state: lint_kind -> t -> lint_state
(* True iff get_state returns Warn or Err, false otherwise *)
val is_enabled: lint_kind -> t -> bool
(* Always the logical opposite of is_enabled *)
val is_suppressed: lint_kind -> t -> bool
(* Get the location of the comment that set the value for a lint kind, or none if
 * the active value was not set by a comment *)
val get_loc: lint_kind -> t -> Loc.t option
(* Iterate over all lint kinds with an explicit setting *)
val iter: (lint_kind -> lint_state * Loc.t option -> unit) -> t -> unit
(* Fold over all lint kinds with an explicit setting *)
val fold: (lint_kind -> lint_state * Loc.t option -> 'a -> 'a) -> t -> 'a -> 'a
(* Map over all lint kinds with an explicit setting *)
val map: (lint_state * Loc.t option -> lint_state * Loc.t option) -> t -> t
(* Merge two LintSettings, with rules in higher_precedence overwriting
 * rules in lower_precedencse. *)
val merge: low_prec:t -> high_prec:t -> t

val of_lines: t -> (int * string) list -> (t, int * string) result
