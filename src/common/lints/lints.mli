(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type sketchy_null_kind =
 | SketchyNullBool
 | SketchyNullString
 | SketchyNullNumber
 | SketchyNullBigInt
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
  | DeprecatedEnumUtility
  | DynamicExport
  | UnsafeGettersSetters
  | InexactSpread
  | UnnecessaryOptionalChain
  | UnnecessaryInvariant
  | SignatureVerificationFailure
  | ImplicitInexactObject
  | UninitializedInstanceProperty

val string_of_kind: lint_kind -> string

val kinds_of_string: string -> lint_kind list option

module LintMap: MyMap.S with type key = lint_kind
module LintSet: Set.S with type elt = lint_kind
