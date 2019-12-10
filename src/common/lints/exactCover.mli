(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * An ExactCover.t represents an exact cover over 1 file.
 *
 * See https://en.wikipedia.org/wiki/Exact_cover; the short version is that an
 * ExactCover.t represents a set of adjacent, nonoverlapping, nonempty,
 * exhaustive ranges over 0 or more files.
 *
 * ExactCover.t is a read-only structure for efficient querying.
 *
 * ExactCover.builder is a write-only structure for efficient construction
 * (under some assuptions) that can be baked into an ExactCover.t.
 *)

open Lints
open Severity

exception Uncovered of string

(* Supports O(log(n)) queries to get the value associated with a loc. *)
type 'a t

(* Given a filename and a value, generate a cover associating that value with that entire file. *)
val file_cover : File_key.t -> 'a -> 'a t

(* Gets the value associated with a certain location in the code. To resolve
 * ambiguity, this looks at the location of the first character in the provided
 * location. Errors if queried for a file not contained in this cover. *)
val find : Loc.t -> 'a t -> 'a

val find_opt : Loc.t -> 'a t -> 'a option

(* Supports O(j*(j+k)) operations to modify a range of a cover being constructed,
 * where j is the number of ranges in the builder intersecting the range being
 * modified and k is the number of ranges in the builder after the range being
 * modified. Note that this means that operations are O(1) in the case where
 * ranges are modified in order from the beginning of the file to the end, which
 * is how this builder is used. *)
(* Better complexity modifications are possible, but because the builder is only
 * used to modify ranges in order, giving O(1) complexity, there was no reason
 * to make them. *)
(* The builder structure is only intended to construct a cover for a single
 * file. If a cover for multiple files is needed, construct and bake a cover for
 * each file and use the union functions to combine them. *)
type 'a builder

(* Create a new builder for the provided file. The resultant builder is an exact
 * cover with a single range associating the whole file with the provided value. *)
val new_builder : File_key.t -> 'a -> 'a builder

(* Change the value in the provided range by applying the provided mapping
 * function. Ranges in the builder that are completely contained in the provided
 * range have their values replaced using the mapping function. Ranges in the
 * builder that have no overlap with the provided range are left untouched.
 * Ranges in the builder that intersect with the provided range are split into
 * two ranges: one that is completely contained in the provided range and one
 * that has no overlap with the provided range. If the provided range is
 * completely contained within a range in the builder, the range in the builder
 * is split into three ranges: one that matches the provided range and two that
 * have no overlap wth the provided range. *)
val update_range : Loc.t -> ('a -> 'a) -> 'a builder -> 'a builder

(* Change the settings in the provided range by adding the provided settings list.
* In the settings list, the kind is the type of lint, the value is the value to set to,
* and the location is the position of the setting in the source code. *)
val update_settings :
  Loc.t ->
  (* Range to operate on *)
  (lint_kind * ('a * Loc.t)) list ->
  (* List of settings to add *)
  'a LintSettings.t builder ->
  (* Builder to work from *)
  'a LintSettings.t builder

(* Resultant builder *)

(* Works similarly to update_settings, but takes two additional parameters: a running
 * LintSettings object and an error handling function. The LintSettings object is updated with
 * the new lint settings (in addition to the builder being updated), and if any redundant
 * settings are encountered, the error function is called with an error message and the
 * location of the error. Additionally, takes the lint settings in unflattened form so
 * that errors can be properly reported. *)
(* This function only checks for settings that are redundant because they don't change
 * anything. It doesn't check for settings that are redundant because they are
 * immediately overwritten. (That's done elsewhere.) *)
val update_settings_and_running :
  'a LintSettings.t ->
  ((* Running lint settings *)
   Loc.t * LintSettings.lint_parse_error -> unit) ->
  (* Parse error handler *)
  Loc.t ->
  (* Range to operate on *)
  (lint_kind * ('a * Loc.t)) list list ->
  (* Unflattened list of settings to add *)
  'a LintSettings.t builder ->
  (* Builder to work from *)
  'a LintSettings.t builder (* Resultant builder *) * 'a LintSettings.t

(* Resultant running lint settings *)

val bake : 'a builder -> 'a t

(* `severity LintSettings.t`-specific functions *)

type lint_severity_cover = severity LintSettings.t t

(* Given a filename, generate a cover that applies the default lint severities
 * across the entire file. *)
val default_file_cover : File_key.t -> lint_severity_cover

(* Gets the severity of the provided lint kind at the provided location. Errors
 * if queried for a file not contained in this cover. *)
val get_severity : lint_kind -> Loc.t -> lint_severity_cover -> severity

(* True iff the provided lint kind has severity `Off` at the provided location.
 * Errors if queried for a file not contained in this cover. *)
val is_suppressed : lint_kind -> Loc.t -> lint_severity_cover -> bool

(* True iff the severity for the provided lint kind has been explicitly set at
 * the provided location. Errors if queried for a file not contained in this
 * cover. *)
val is_explicit : lint_kind -> Loc.t -> lint_severity_cover -> bool

(* Intended for debugging purposes. *)
val to_string : lint_severity_cover -> string
