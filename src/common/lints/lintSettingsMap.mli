(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Supports O(log(n)) queries to get the relevant suppression for a loc. *)
type t

(* Given a filename and a settings, generate a suppression map that applies those
 * settings across the entire file. *)
val global_settings: Loc.filename -> LintSettings.t -> t
(* Given a filename, generate a suppression map that applies the default settings
 * across the entire file. *)
val default_settings: Loc.filename -> t
(* This isn't a particularly valid suppression map, but it's fine as long as
 * no-one tries to use it. *)
val invalid_default: t
(* Gets the lint settings that apply to a certain location in the code. To
 * resolve ambiguity, this looks at the location of the first character in the
 * provided location. *)
val settings_at_loc: Loc.t -> t -> LintSettings.t

val get_state: LintSettings.lint_kind -> Loc.t -> t -> LintSettings.lint_state

val is_suppressed: LintSettings.lint_kind -> Loc.t -> t -> bool
(* True iff the severity for the provided lint has been explicitly set *)
val is_explicit: LintSettings.lint_kind -> Loc.t -> t -> bool

val union: t -> t -> t
(* combines settings collated by filename into one collection *)
val union_settings : t Utils_js.FilenameMap.t -> t


(* Supports O(m) operations to add a range of suppression rules, where m is
 * the number of rules. (assuming that the rules are processed in order) *)
 (* These functions were built with the case of all ranges belonging to the same file
  * in mind. It may or may not work in other cases. *)
type builder
(* Create a new builder for the provided file, using the provided base settings *)
val new_builder: Loc.filename -> LintSettings.t -> builder
(* Change the settings in the provided range by adding the provided settings list.
 * In the settings list, the kind is the type of lint, the lint_state is the state to set to,
 * and the location is the position of the setting in the source code. *)
val update_settings:
  Loc.t -> (LintSettings.lint_kind * (LintSettings.lint_state * Loc.t)) list -> builder -> builder
(* Works similarly to update_settings, but takes two additional parameters: a running
 * LintSettings object and an error function. The LintSettings object is updated with
 * the new lint settings (in addition to the builder being updated), and if any redundant
 * settings are encountered, the error function is called with an error message and the
 * location of the error. Additionally, takes the lint settings in unflattened form so
 * that errors can be properly reported. *)
(* This function only checks for settings that are redundant because they don't change
 * anything. It doesn't check for settings that are redundant because they are
 * immediately overwritten. (That's done elsewhere.) *)
val update_settings_and_running:
  LintSettings.t ->
  (LintSettings.error -> unit) ->
  Loc.t ->
  (LintSettings.lint_kind * (LintSettings.lint_state * Loc.t)) list list ->
  builder ->
  builder * LintSettings.t

val bake: builder -> t
