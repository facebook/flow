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
  version: Semver_version.t;
}

let string_of_op = function
  | Greater -> ">"
  | GreaterOrEqual -> ">="
  | Less -> "<"
  | LessOrEqual -> "<="
  | Equal -> "="

let to_string { op; version } =
  let op =
    match op with
    | Some op -> string_of_op op
    | None -> ""
  in
  op ^ Semver_version.to_string version

let satisfies version { op; version = range } =
  let result = Semver_version.compare_precedence version range in
  match op with
  | Some Greater -> result > 0
  | Some GreaterOrEqual -> result >= 0
  | Some Less -> result < 0
  | Some LessOrEqual -> result <= 0
  | Some Equal
  | None ->
    result = 0
