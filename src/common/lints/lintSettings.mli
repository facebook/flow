(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lints
open Severity

type 'a t

val of_default : severity -> severity t

val set_value : lint_kind -> 'a * Loc.t option -> 'a t -> 'a t

val set_all : (lint_kind * ('a * Loc.t option)) list -> 'a t -> 'a t

val get_default : 'a t -> 'a

(* Get the state of a lint kind in the provided settings *)
val get_value : lint_kind -> 'a t -> 'a

(* True iff the severity for the provided lint has been explicitly set *)
val is_explicit : lint_kind -> 'a t -> bool

(* Get the location of the comment that set the value for a lint kind, or none if
 * the active value was not set by a comment *)
val get_loc : lint_kind -> 'a t -> Loc.t option

(* Iterate over all lint kinds with an explicit value *)
val iter : (lint_kind -> 'a * Loc.t option -> unit) -> 'a t -> unit

(* Fold over all lint kinds with an explicit value *)
val fold : (lint_kind -> 'a * Loc.t option -> 'b -> 'b) -> 'a t -> 'b -> 'b

(* Map over all lint kinds with an explicit value *)
val map : ('a * Loc.t option -> 'a * Loc.t option) -> 'a t -> 'a t

val default_lint_severities : (lint_kind * (severity * 'a option)) list

(* SEVERITY-SPECIFIC FUNCTIONS *)

val empty_severities : severity t

(* True iff get_state returns Warn or Err, false otherwise *)
val is_enabled : lint_kind -> severity t -> bool

(* Always the logical opposite of is_enabled *)
val is_suppressed : lint_kind -> severity t -> bool

val of_lines : severity t -> (int * string) list -> (severity t, int * string) result

(* Intended for debugging purposes. *)
val to_string : severity t -> string

type lint_parse_error_kind =
  | Invalid_setting
  | Malformed_argument
  | Naked_comment
  | Nonexistent_rule
  | Overwritten_argument
  | Redundant_argument

type lint_parse_error = Loc.t * lint_parse_error_kind
