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

val string_of_kind: lint_kind -> string

val kinds_of_string: string -> lint_kind list option

module LintMap: MyMap.S with type key = lint_kind
