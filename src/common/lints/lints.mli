(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  | SketchyNullEnumBool
  | SketchyNullEnumString
  | SketchyNullEnumNumber
  | SketchyNullEnumBigInt

type sketchy_number_kind = SketchyNumberAnd

type property_assignment_kind =
  | PropertyNotDefinitelyInitialized
  | ReadFromUninitializedProperty
  | MethodCallBeforeEverythingInitialized
  | ThisBeforeEverythingInitialized
  | PropertyFunctionCallBeforeEverythingInitialized

type deprecated_type_kind =
  | DeprecatedBool
  | DeprecatedStar

type lint_kind =
  | SketchyNull of sketchy_null_kind
  | SketchyNumber of sketchy_number_kind
  | UntypedTypeImport
  | UntypedImport
  | NonstrictImport
  | UnclearType
  | DeprecatedType of deprecated_type_kind
  | UnsafeGettersSetters
  | UnnecessaryOptionalChain
  | UnnecessaryInvariant
  | ImplicitInexactObject
  | UninitializedInstanceProperty
  | AmbiguousObjectType
  | RequireExplicitEnumSwitchCases
  | DefaultImportAccess
  | InvalidImportStarUse
  | NonConstVarExport
  | ThisInExportedFunction
  | MixedImportAndRequire
  | ExportRenamedDefault
  | UnusedPromiseInAsyncScope

val string_of_kind : lint_kind -> string

val kinds_of_string : string -> lint_kind list option

module LintMap : WrappedMap.S with type key = lint_kind

module LintSet : Flow_set.S with type elt = lint_kind
