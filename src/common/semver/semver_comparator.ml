(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type op =
| Greater
| GreaterOrEqual
| Less
| LessOrEqual
| Equal

type t = {
  op: op option;
  version: Semver_version.t
}

let string_of_op = function
  | Greater -> ">"
  | GreaterOrEqual -> ">="
  | Less -> "<"
  | LessOrEqual -> "<="
  | Equal -> "="

let to_string { op; version } =
  let op = match op with
  | Some op -> string_of_op op
  | None -> ""
  in
  op ^ (Semver_version.to_string version)

(* Determines if the version is matched by the range.
 *
 * If the range and the version both have a prerelease, then they must be for the same
 * version (major, minor, patch). for example, `>1.2.3-alpha` matches `1.2.3-beta` and
 * `1.2.4`, but not `1.2.4-alpha`. this is so that opting into one prerelease version
 * does not also opt you into all future prereleases.
 *)
let satisfies version {op; version = range} =
  let result = Semver_version.compare_precedence version range in
  let pass = match op with
  | Some Greater -> result > 0
  | Some GreaterOrEqual -> result >= 0
  | Some Less -> result < 0
  | Some LessOrEqual -> result <= 0
  | Some Equal
  | None -> result = 0
  in
  if not pass then false
  else
    let open Semver_version in
    match version, range with
    | { major = a_major; minor = a_minor; patch = a_patch; prerelease = _::_; build = _ },
      { major = b_major; minor = b_minor; patch = b_patch; prerelease = _::_; build = _ } ->
        a_major = b_major && a_minor = b_minor && a_patch = b_patch
    | _ -> true
