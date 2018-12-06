(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type sketchy_null_kind =
  | SketchyNullBool
  | SketchyNullString
  | SketchyNullNumber
  | SketchyNullMixed

type sketchy_number_kind =
  | SketchyNumberAnd

type lint_kind =
  | SketchyNull of sketchy_null_kind
  | SketchyNumber of sketchy_number_kind
  | UntypedTypeImport
  | UntypedImport
  | NonstrictImport
  | UnclearType
  | DeprecatedType
  | DeprecatedUtility
  | UnsafeGettersSetters
  | InexactSpread
  | UnnecessaryOptionalChain
  | UnnecessaryInvariant
  | DeprecatedCallSyntax
  | SignatureVerificationFailure

let string_of_sketchy_null_kind = function
  | SketchyNullBool -> "sketchy-null-bool"
  | SketchyNullString -> "sketchy-null-string"
  | SketchyNullNumber -> "sketchy-null-number"
  | SketchyNullMixed -> "sketchy-null-mixed"

let string_of_sketchy_number_kind = function
  | SketchyNumberAnd -> "sketchy-number-and"

let string_of_kind = function
  | SketchyNull kind -> string_of_sketchy_null_kind kind
  | SketchyNumber kind -> string_of_sketchy_number_kind kind
  | UntypedTypeImport -> "untyped-type-import"
  | UntypedImport -> "untyped-import"
  | NonstrictImport -> "nonstrict-import"
  | UnclearType -> "unclear-type"
  | DeprecatedType -> "deprecated-type"
  | DeprecatedUtility -> "deprecated-utility"
  | UnsafeGettersSetters -> "unsafe-getters-setters"
  | InexactSpread -> "inexact-spread"
  | UnnecessaryOptionalChain -> "unnecessary-optional-chain"
  | UnnecessaryInvariant -> "unnecessary-invariant"
  | DeprecatedCallSyntax -> "deprecated-call-syntax"
  | SignatureVerificationFailure -> "signature-verification-failure"

let kinds_of_string = function
  | "sketchy-null" -> Some [
      SketchyNull SketchyNullBool;
      SketchyNull SketchyNullString;
      SketchyNull SketchyNullNumber;
      SketchyNull SketchyNullMixed;
    ]
  | "sketchy-null-bool" -> Some [SketchyNull SketchyNullBool]
  | "sketchy-null-string" -> Some [SketchyNull SketchyNullString]
  | "sketchy-null-number" -> Some [SketchyNull SketchyNullNumber]
  | "sketchy-null-mixed" -> Some [SketchyNull SketchyNullMixed]
  | "sketchy-number" -> Some [
      SketchyNumber SketchyNumberAnd;
    ]
  | "sketchy-number-and" -> Some [SketchyNumber SketchyNumberAnd]
  | "untyped-type-import" -> Some [UntypedTypeImport]
  | "nonstrict-import" -> Some [NonstrictImport]
  | "untyped-import" -> Some [UntypedImport]
  | "unclear-type" -> Some [UnclearType]
  | "deprecated-type" -> Some [DeprecatedType]
  | "deprecated-utility" -> Some [DeprecatedUtility]
  | "unsafe-getters-setters" -> Some [UnsafeGettersSetters]
  | "inexact-spread" -> Some [InexactSpread]
  | "unnecessary-optional-chain" -> Some [UnnecessaryOptionalChain]
  | "unnecessary-invariant" -> Some [UnnecessaryInvariant]
  | "deprecated-call-syntax" -> Some [DeprecatedCallSyntax]
  | "signature-verification-failure" -> Some [SignatureVerificationFailure]
  | _ -> None

module LintKind = struct
  type t = lint_kind
  let compare = compare
end

module LintMap = MyMap.Make(LintKind)
module LintSet = Set.Make(LintKind)
