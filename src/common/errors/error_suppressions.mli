(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t_map

val empty_map : t_map

(* Raises if the given loc has `source` set to `None` *)
val add_to_map : Loc.t -> t_map -> t_map
val add_lint_suppressions_to_map : Utils_js.LocSet.t -> t_map -> t_map

val remove_from_map : File_key.t -> t_map -> t_map

(* Union the two given maps. If they both contain values for a given key, union the values. *)
val union_maps : t_map -> t_map -> t_map
(* Union the two given maps. If they both contain values for a given key, use the value from the
 * second argument. If this would result in an empty value, removing the key/value pair altogether.
 *)
val update_suppressions: t_map -> t_map -> t_map

val all_locs_of_map : t_map -> Loc.t list

val filter_suppressed_errors :
  t_map -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t -> Errors.ErrorSet.t -> unused:t_map ->
  (Errors.ErrorSet.t * Errors.ErrorSet.t * (Errors.error * Utils_js.LocSet.t) list * t_map)
