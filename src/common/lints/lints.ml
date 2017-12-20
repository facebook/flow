(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type sketchy_null_kind =
  | SketchyBool
  | SketchyString
  | SketchyNumber
  | SketchyMixed

type lint_kind =
  | SketchyNull of sketchy_null_kind
  | UntypedTypeImport
  | UntypedImport
  | NonstrictImport
  | UnclearType
  | UnsafeGettersSetters
  | DeprecatedDeclareExports

let string_of_sketchy_null_kind = function
  | SketchyBool -> "sketchy-null-bool"
  | SketchyString -> "sketchy-null-string"
  | SketchyNumber -> "sketchy-null-number"
  | SketchyMixed -> "sketchy-null-mixed"

let string_of_kind = function
  | SketchyNull kind -> string_of_sketchy_null_kind kind
  | UntypedTypeImport -> "untyped-type-import"
  | UntypedImport -> "untyped-import"
  | NonstrictImport -> "nonstrict-import"
  | UnclearType -> "unclear-type"
  | UnsafeGettersSetters -> "unsafe-getters-setters"
  | DeprecatedDeclareExports -> "deprecated-declare-exports"

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
  | "nonstrict-import" -> Some [NonstrictImport]
  | "untyped-import" -> Some [UntypedImport]
  | "unclear-type" -> Some [UnclearType]
  | "unsafe-getters-setters" -> Some [UnsafeGettersSetters]
  | "deprecated-declare-exports" -> Some [DeprecatedDeclareExports]
  | _ -> None

module LintKind = struct
  type t = lint_kind
  let compare = compare
end

module LintMap = MyMap.Make(LintKind)
module LintSet = Set.Make(LintKind)
