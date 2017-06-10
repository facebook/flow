(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
type lint_kind =
  | SketchyNullBool
  | SketchyNullString
  | SketchyNullNumber
  | SketchyNullMixed

let string_of_kind = function
  | SketchyNullBool -> "sketchy_null_bool"
  | SketchyNullString -> "sketchy_null_string"
  | SketchyNullNumber -> "sketchy_null_number"
  | SketchyNullMixed -> "sketchy_null_mixed"

module LintKind = struct
  type t = lint_kind
  let compare = compare
end

module LintSet = Set.Make(LintKind)

type t = {
  default_err: bool; (* Whether to throw an error if the error kind isn't found in the map *)
  exception_set: LintSet.t (* The exceptions to the default rule *)
}

let default_settings = {
  default_err = false;
  exception_set = LintSet.empty
}

let fresh_settings default_err = {default_settings with default_err}

let set_enabled key value settings =
  if value != settings.default_err then
    let new_set = LintSet.add key settings.exception_set
    in {settings with exception_set = new_set}
  else settings

let get_default settings = settings.default_err

let is_suppressed lint_kind settings =
  if LintSet.mem lint_kind settings.exception_set then
    settings.default_err
  else
    not settings.default_err

(* Iterates over all lint kinds that are not implicitly set by the default *)
let iter f settings =
  let f kind = f kind (not settings.default_err) in
  LintSet.iter f settings.exception_set

let string_to_lints = function
  | "sketchy_null" -> [SketchyNullBool; SketchyNullString; SketchyNullNumber; SketchyNullMixed]
  | "sketchy_null_bool" -> [SketchyNullBool]
  | "sketchy_null_string" -> [SketchyNullString]
  | "sketchy_null_number" -> [SketchyNullNumber]
  | "sketchy_null_mixed" -> [SketchyNullMixed]
  | _ -> raise Not_found
