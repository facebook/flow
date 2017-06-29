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

val empty : t
val is_empty : t -> bool
val add : Loc.t -> t -> t
val union : t -> t -> t
val set_unused_lint_suppressions : Loc.LocSet.t -> t -> t
val check : Errors.error -> SuppressionMap.t -> t -> (bool * Loc.LocSet.t * t)
val unused : t -> Loc.t list

(* combines suppressions collated by filename into one collection *)
val union_suppressions : t Utils_js.FilenameMap.t -> t

val filter_suppressed_errors :
  t -> SuppressionMap.t -> Errors.ErrorSet.t ->
  (Errors.ErrorSet.t * (Errors.error * Loc.LocSet.t) list * t)
