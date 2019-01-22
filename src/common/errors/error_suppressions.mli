(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty: t

(* Raises if the given loc has `source` set to `None` *)
val add: Loc.t -> t -> t
val add_lint_suppressions: Utils_js.LocSet.t -> t -> t

val remove: File_key.t -> t -> t

(* Union the two collections of suppressions. If they both contain suppressions for a given file,
 * include both sets of suppressions. *)
val union: t -> t -> t
(* Union the two collections of suppressions. If they both contain suppressions for a given file,
 * discard those included in the first argument. *)
val update_suppressions: t -> t -> t

val all_locs: t -> Loc.t list

val filter_suppressed_errors :
  t -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t -> Errors.ErrorSet.t -> unused:t ->
  (Errors.ErrorSet.t * Errors.ErrorSet.t * (ALoc.t Errors.error * Utils_js.LocSet.t) list * t)
