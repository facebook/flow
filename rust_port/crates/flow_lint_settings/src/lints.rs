/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum SketchyNullKind {
    Bool,
    String,
    Number,
    BigInt,
    Mixed,
    EnumBool,
    EnumString,
    EnumNumber,
    EnumBigInt,
}

impl SketchyNullKind {
    pub fn as_str(self) -> &'static str {
        match self {
            SketchyNullKind::Bool | SketchyNullKind::EnumBool => "sketchy-null-bool",
            SketchyNullKind::String | SketchyNullKind::EnumString => "sketchy-null-string",
            SketchyNullKind::Number | SketchyNullKind::EnumNumber => "sketchy-null-number",
            SketchyNullKind::BigInt | SketchyNullKind::EnumBigInt => "sketchy-null-bigint",
            SketchyNullKind::Mixed => "sketchy-null-mixed",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum SketchyNumberKind {
    And,
}

impl SketchyNumberKind {
    pub fn as_str(self) -> &'static str {
        match self {
            SketchyNumberKind::And => "sketchy-number-and",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum PropertyAssignmentKind {
    PropertyNotDefinitelyInitialized,
    ReadFromUninitializedProperty,
    MethodCallBeforeEverythingInitialized,
    ThisBeforeEverythingInitialized,
    PropertyFunctionCallBeforeEverythingInitialized,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DeprecatedTypeKind {
    Bool,
}

impl DeprecatedTypeKind {
    pub fn as_str(self) -> &'static str {
        match self {
            DeprecatedTypeKind::Bool => "deprecated-type-bool",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum LintKind {
    UntypedTypeImport,
    UntypedImport,
    NonstrictImport,
    InternalType,
    UnclearType,
    UnsafeGettersSetters,
    UnsafeObjectAssign,
    UnnecessaryOptionalChain,
    UnnecessaryInvariant,
    ImplicitInexactObject,
    UninitializedInstanceProperty,
    AmbiguousObjectType,
    RequireExplicitEnumChecks,
    RequireExplicitEnumSwitchCases,
    DefaultImportAccess,
    InvalidImportStarUse,
    NonConstVarExport,
    ThisInExportedFunction,
    MixedImportAndRequire,
    ExportRenamedDefault,
    UnusedPromise,
    ReactIntrinsicOverlap,
    NestedComponent,
    NestedHook,
    LibdefOverride,
    SketchyNull(SketchyNullKind),
    SketchyNumber(SketchyNumberKind),
    DeprecatedType(DeprecatedTypeKind),
}

impl LintKind {
    pub fn as_str(self) -> &'static str {
        match self {
            LintKind::SketchyNull(k) => k.as_str(),
            LintKind::SketchyNumber(k) => k.as_str(),
            LintKind::UntypedTypeImport => "untyped-type-import",
            LintKind::UntypedImport => "untyped-import",
            LintKind::NonstrictImport => "nonstrict-import",
            LintKind::InternalType => "internal-type",
            LintKind::UnclearType => "unclear-type",
            LintKind::DeprecatedType(k) => k.as_str(),
            LintKind::UnsafeGettersSetters => "unsafe-getters-setters",
            LintKind::UnsafeObjectAssign => "unsafe-object-assign",
            LintKind::UnnecessaryOptionalChain => "unnecessary-optional-chain",
            LintKind::UnnecessaryInvariant => "unnecessary-invariant",
            LintKind::ImplicitInexactObject => "implicit-inexact-object",
            LintKind::UninitializedInstanceProperty => "uninitialized-instance-property",
            LintKind::AmbiguousObjectType => "ambiguous-object-type",
            LintKind::RequireExplicitEnumChecks => "require-explicit-enum-checks",
            LintKind::RequireExplicitEnumSwitchCases => "require-explicit-enum-switch-cases",
            LintKind::DefaultImportAccess => "default-import-access",
            LintKind::InvalidImportStarUse => "invalid-import-star-use",
            LintKind::NonConstVarExport => "non-const-var-export",
            LintKind::ThisInExportedFunction => "this-in-exported-function",
            LintKind::MixedImportAndRequire => "mixed-import-and-require",
            LintKind::ExportRenamedDefault => "export-renamed-default",
            LintKind::UnusedPromise => "unused-promise",
            LintKind::ReactIntrinsicOverlap => "react-intrinsic-overlap",
            LintKind::NestedComponent => "nested-component",
            LintKind::NestedHook => "nested-hook",
            LintKind::LibdefOverride => "libdef-override",
        }
    }

    pub fn parse_from_str(s: &str) -> Option<Vec<LintKind>> {
        match s {
            "sketchy-null" => Some(vec![
                LintKind::SketchyNull(SketchyNullKind::Bool),
                LintKind::SketchyNull(SketchyNullKind::String),
                LintKind::SketchyNull(SketchyNullKind::Number),
                LintKind::SketchyNull(SketchyNullKind::BigInt),
                LintKind::SketchyNull(SketchyNullKind::Mixed),
                LintKind::SketchyNull(SketchyNullKind::EnumBool),
                LintKind::SketchyNull(SketchyNullKind::EnumString),
                LintKind::SketchyNull(SketchyNullKind::EnumNumber),
                LintKind::SketchyNull(SketchyNullKind::EnumBigInt),
            ]),
            "sketchy-null-bool" => Some(vec![
                LintKind::SketchyNull(SketchyNullKind::Bool),
                LintKind::SketchyNull(SketchyNullKind::EnumBool),
            ]),
            "sketchy-null-string" => Some(vec![
                LintKind::SketchyNull(SketchyNullKind::String),
                LintKind::SketchyNull(SketchyNullKind::EnumString),
            ]),
            "sketchy-null-number" => Some(vec![
                LintKind::SketchyNull(SketchyNullKind::Number),
                LintKind::SketchyNull(SketchyNullKind::EnumNumber),
            ]),
            "sketchy-null-bigint" => Some(vec![
                LintKind::SketchyNull(SketchyNullKind::BigInt),
                LintKind::SketchyNull(SketchyNullKind::EnumBigInt),
            ]),
            "sketchy-null-mixed" => Some(vec![LintKind::SketchyNull(SketchyNullKind::Mixed)]),
            "sketchy-number" => Some(vec![LintKind::SketchyNumber(SketchyNumberKind::And)]),
            "sketchy-number-and" => Some(vec![LintKind::SketchyNumber(SketchyNumberKind::And)]),
            "untyped-type-import" => Some(vec![LintKind::UntypedTypeImport]),
            "nonstrict-import" => Some(vec![LintKind::NonstrictImport]),
            "untyped-import" => Some(vec![LintKind::UntypedImport]),
            "internal-type" => Some(vec![LintKind::InternalType]),
            "unclear-type" => Some(vec![LintKind::UnclearType]),
            "deprecated-type" => Some(vec![LintKind::DeprecatedType(DeprecatedTypeKind::Bool)]),
            "deprecated-type-bool" => {
                Some(vec![LintKind::DeprecatedType(DeprecatedTypeKind::Bool)])
            }
            "unsafe-getters-setters" => Some(vec![LintKind::UnsafeGettersSetters]),
            "unsafe-object-assign" => Some(vec![LintKind::UnsafeObjectAssign]),
            "unnecessary-optional-chain" => Some(vec![LintKind::UnnecessaryOptionalChain]),
            "unnecessary-invariant" => Some(vec![LintKind::UnnecessaryInvariant]),
            "implicit-inexact-object" => Some(vec![LintKind::ImplicitInexactObject]),
            "ambiguous-object-type" => Some(vec![LintKind::AmbiguousObjectType]),
            "require-explicit-enum-checks" => Some(vec![LintKind::RequireExplicitEnumChecks]),
            "require-explicit-enum-switch-cases" => {
                Some(vec![LintKind::RequireExplicitEnumSwitchCases])
            }
            "uninitialized-instance-property" => {
                Some(vec![LintKind::UninitializedInstanceProperty])
            }
            "default-import-access" => Some(vec![LintKind::DefaultImportAccess]),
            "invalid-import-star-use" => Some(vec![LintKind::InvalidImportStarUse]),
            "non-const-var-export" => Some(vec![LintKind::NonConstVarExport]),
            "this-in-exported-function" => Some(vec![LintKind::ThisInExportedFunction]),
            "mixed-import-and-require" => Some(vec![LintKind::MixedImportAndRequire]),
            "export-renamed-default" => Some(vec![LintKind::ExportRenamedDefault]),
            "unused-promise" => Some(vec![LintKind::UnusedPromise]),
            "react-intrinsic-overlap" => Some(vec![LintKind::ReactIntrinsicOverlap]),
            "nested-component" => Some(vec![LintKind::NestedComponent]),
            "nested-hook" => Some(vec![LintKind::NestedHook]),
            "libdef-override" => Some(vec![LintKind::LibdefOverride]),
            _ => None,
        }
    }
}
