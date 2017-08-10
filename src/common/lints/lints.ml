(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type sketchy_null_kind =
  | SketchyBool
  | SketchyString
  | SketchyNumber
  | SketchyMixed

type lint_kind =
  | SketchyNull of sketchy_null_kind
  | UntypedTypeImport

let string_of_sketchy_null_kind = function
  | SketchyBool -> "sketchy-null-bool"
  | SketchyString -> "sketchy-null-string"
  | SketchyNumber -> "sketchy-null-number"
  | SketchyMixed -> "sketchy-null-mixed"

let string_of_kind = function
  | SketchyNull kind -> string_of_sketchy_null_kind kind
  | UntypedTypeImport -> "untyped-type-import"

let kinds_of_string = function
  | "sketchy-null" -> Some [
      SketchyNull SketchyBool;
      SketchyNull SketchyString;
      SketchyNull SketchyNumber;
      SketchyNull SketchyMixed;
    ]
  | "sketchy-null-bool" -> Some [SketchyNull SketchyBool]
  | "sketchy-null-string" -> Some [SketchyNull SketchyString]
  | "sketchy-null-number" -> Some [SketchyNull SketchyNumber]
  | "sketchy-null-mixed" -> Some [SketchyNull SketchyMixed]
  | "untyped-type-import" -> Some [UntypedTypeImport]
  | _ -> None

module LintKind = struct
  type t = lint_kind
  let compare = compare
end

module LintMap = MyMap.Make(LintKind)
