(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type part =
  | Comparator of Semver_comparator.t
  | Caret of Semver_version.t

(* TODO: support unions (`||`), like this:
type comparator_set = part list
type t = comparator_set list
*)
type t = part list

let expand_caret version =
  let open Semver_version in
  let upper = match version with
  | { major = 0; minor = 0; _ } -> incr_patch version
  | { major = 0; _ } -> incr_minor version
  | _ -> incr_major version
  in
  [ Semver_comparator.({ op = Some GreaterOrEqual; version });
    Semver_comparator.({ op = Some Less; version = upper })]

let fold_comparators_of_range f acc t =
  List.fold_left (fun acc part ->
    match part with
    | Comparator c -> f acc c
    | Caret v -> List.fold_left f acc (expand_caret v)
  ) acc t
let comparators_of_range t : Semver_comparator.t list =
  let comparators = fold_comparators_of_range (fun acc comp -> comp::acc) [] t in
  List.rev comparators

let satisfies range version =
  fold_comparators_of_range (fun acc comp ->
    if not acc then acc
    else Semver_comparator.satisfies version comp
  ) true range

let string_of_part = function
  | Comparator c -> Semver_comparator.to_string c
  | Caret ver -> "^" ^ (Semver_version.to_string ver)

let to_string t =
  t
  |> List.map string_of_part
  |> String.concat " "
