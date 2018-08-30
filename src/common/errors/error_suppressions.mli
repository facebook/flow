(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t
val is_empty : t -> bool
val add : Loc.t -> t -> t
val add_lint_suppressions : Utils_js.LocSet.t -> t -> t
val union : t -> t -> t
val unused : t -> Loc.t list

(* combines suppressions collated by filename into one collection *)
val union_suppressions : t Utils_js.FilenameMap.t -> t

val filter_suppressed_errors :
  t -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t -> Errors.ErrorSet.t -> unused:t ->
  (Errors.ErrorSet.t * Errors.ErrorSet.t * (Errors.error * Utils_js.LocSet.t) list * t)
