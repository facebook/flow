(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type sketchy_null_kind =
  | SketchyNullBool
  | SketchyNullString
  | SketchyNullNumber
  | SketchyNullMixed
  | SketchyNullEnumBool
  | SketchyNullEnumString
  | SketchyNullEnumNumber

type sketchy_number_kind = SketchyNumberAnd

type property_assignment_kind =
  | PropertyNotDefinitelyInitialized
  | ReadFromUninitializedProperty
  | MethodCallBeforeEverythingInitialized
  | ThisBeforeEverythingInitialized
  | PropertyFunctionCallBeforeEverythingInitialized

type lint_kind =
  | SketchyNull of sketchy_null_kind
  | SketchyNumber of sketchy_number_kind
  | UntypedTypeImport
  | UntypedImport
  | NonstrictImport
  | UnclearType
  | DeprecatedType
  | DeprecatedUtility
  | DynamicExport
  | UnsafeGettersSetters
  | UnnecessaryOptionalChain
  | UnnecessaryInvariant
  | SignatureVerificationFailure
  | ImplicitInexactObject
  | UninitializedInstanceProperty
  | AmbiguousObjectType

let string_of_sketchy_null_kind = function
  | SketchyNullBool
  | SketchyNullEnumBool ->
    "sketchy-null-bool"
  | SketchyNullString
  | SketchyNullEnumString ->
    "sketchy-null-string"
  | SketchyNullNumber
  | SketchyNullEnumNumber ->
    "sketchy-null-number"
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
  | DynamicExport -> "dynamic-export"
  | UnsafeGettersSetters -> "unsafe-getters-setters"
  | UnnecessaryOptionalChain -> "unnecessary-optional-chain"
  | UnnecessaryInvariant -> "unnecessary-invariant"
  | SignatureVerificationFailure -> "signature-verification-failure"
  | ImplicitInexactObject -> "implicit-inexact-object"
  | UninitializedInstanceProperty -> "uninitialized-instance-property"
  | AmbiguousObjectType -> "ambiguous-object-type"

let kinds_of_string = function
  | "sketchy-null" ->
    Some
      [
        SketchyNull SketchyNullBool;
        SketchyNull SketchyNullString;
        SketchyNull SketchyNullNumber;
        SketchyNull SketchyNullMixed;
        SketchyNull SketchyNullEnumBool;
        SketchyNull SketchyNullEnumString;
        SketchyNull SketchyNullEnumNumber;
      ]
  | "sketchy-null-bool" -> Some [SketchyNull SketchyNullBool; SketchyNull SketchyNullEnumBool]
  | "sketchy-null-string" -> Some [SketchyNull SketchyNullString; SketchyNull SketchyNullEnumString]
  | "sketchy-null-number" -> Some [SketchyNull SketchyNullNumber; SketchyNull SketchyNullEnumNumber]
  | "sketchy-null-mixed" -> Some [SketchyNull SketchyNullMixed]
  | "sketchy-number" -> Some [SketchyNumber SketchyNumberAnd]
  | "sketchy-number-and" -> Some [SketchyNumber SketchyNumberAnd]
  | "untyped-type-import" -> Some [UntypedTypeImport]
  | "nonstrict-import" -> Some [NonstrictImport]
  | "untyped-import" -> Some [UntypedImport]
  | "unclear-type" -> Some [UnclearType]
  | "deprecated-type" -> Some [DeprecatedType]
  | "deprecated-utility" -> Some [DeprecatedUtility]
  | "dynamic-export" -> Some [DynamicExport]
  | "unsafe-getters-setters" -> Some [UnsafeGettersSetters]
  | "unnecessary-optional-chain" -> Some [UnnecessaryOptionalChain]
  | "unnecessary-invariant" -> Some [UnnecessaryInvariant]
  | "signature-verification-failure" -> Some [SignatureVerificationFailure]
  | "implicit-inexact-object" -> Some [ImplicitInexactObject]
  | "ambiguous-object-type" -> Some [AmbiguousObjectType]
  | "uninitialized-instance-property" -> Some [UninitializedInstanceProperty]
  | _ -> None

module LintKind = struct
  type t = lint_kind

  let compare = compare
end

module LintMap = WrappedMap.Make (LintKind)
module LintSet = Set.Make (LintKind)
