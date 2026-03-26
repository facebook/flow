/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use crate::enum_common::ExplicitType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MatchNonLastRestKind {
    Object,
    Array,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TSClassVisibilityKind {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseError {
    AbstractMethodInNonAbstractClass,
    AbstractMethodWithBody,
    AbstractPropertyInNonAbstractClass,
    AbstractPropertyWithInitializer,
    AccessorDataProperty,
    AccessorGetSet,
    AdjacentJSXElements,
    AmbiguousLetBracket,
    AsyncFunctionAsStatement,
    AwaitAsIdentifierReference,
    AwaitInAsyncFormalParameters,
    ComputedShorthandProperty,
    ConstructorCannotBeAccessor,
    ConstructorCannotBeAsync,
    ConstructorCannotBeGenerator,
    ConstructorCannotBeOptional,
    DeclareAsync,
    DeclareAsyncComponent,
    DeclareAsyncHook,
    DeclareClassElement,
    DeclareClassFieldInitializer,
    DeclareOpaqueTypeInitializer,
    DuplicateConstructor,
    DuplicateExport(String),
    DuplicatePrivateFields(String),
    ElementAfterRestElement,
    EnumBigIntMemberNotInitialized {
        enum_name: String,
        member_name: String,
    },
    EnumBooleanMemberNotInitialized {
        enum_name: String,
        member_name: String,
    },
    EnumDuplicateMemberName {
        enum_name: String,
        member_name: String,
    },
    EnumInconsistentMemberValues {
        enum_name: String,
    },
    EnumInvalidEllipsis {
        trailing_comma: bool,
    },
    EnumInvalidExplicitType {
        enum_name: String,
        supplied_type: Option<String>,
    },
    EnumInvalidExport,
    EnumInvalidInitializerSeparator {
        member_name: String,
    },
    EnumInvalidMemberInitializer {
        enum_name: String,
        explicit_type: Option<ExplicitType>,
        member_name: String,
    },
    EnumInvalidMemberSeparator,
    EnumNumberMemberNotInitialized {
        enum_name: String,
        member_name: String,
    },
    EnumStringMemberInconsistentlyInitialized {
        enum_name: String,
    },
    ExpectedJSXClosingTag(String),
    ExpectedPatternFoundExpression,
    ExportSpecifierMissingComma,
    FunctionAsStatement {
        in_strict_mode: bool,
    },
    GeneratorFunctionAsStatement,
    GetterArity,
    GetterMayNotHaveThisParam,
    IllegalBreak {
        in_match_statement: bool,
    },
    IllegalContinue,
    IllegalReturn,
    IllegalUnicodeEscape,
    ImportAttributeMissingComma,
    ImportSpecifierMissingComma,
    ImportTypeShorthandOnlyInPureImport,
    InexactInsideExact,
    InexactInsideNonObject,
    InvalidClassMemberName {
        name: String,
        static_: bool,
        method_: bool,
        private_: bool,
    },
    InvalidComponentParamName,
    InvalidComponentRenderAnnotation {
        has_nested_render: bool,
    },
    InvalidComponentStringParameterBinding {
        optional: bool,
        name: String,
    },
    InvalidFloatBigInt,
    InvalidIndexedAccess {
        has_bracket: bool,
    },
    InvalidJSXAttributeValue,
    InvalidLHSInAssignment,
    InvalidLHSInExponentiation,
    InvalidLHSInForIn,
    InvalidLHSInForOf,
    InvalidOptionalIndexedAccess,
    InvalidRegExp,
    InvalidRegExpFlags(String),
    InvalidSciBigInt,
    InvalidTupleOptionalSpread,
    InvalidTupleVariance,
    InvalidTypeof,
    JSXAttributeValueEmptyExpression,
    LiteralShorthandProperty,
    MalformedUnicode,
    MatchNonLastRest(MatchNonLastRestKind),
    MatchEmptyArgument,
    MatchSpreadArgument,
    MatchExpressionAwait,
    MatchExpressionYield,
    MethodInDestructuring,
    MissingJSXClosingTag(String),
    MissingTypeParam,
    MissingTypeParamDefault,
    MultipleDefaultsInSwitch,
    NewlineAfterThrow,
    NewlineBeforeArrow,
    NoCatchOrFinally,
    NoUninitializedConst,
    NoUninitializedDestructuring,
    NullishCoalescingUnexpectedLogical(String),
    OptionalChainNew,
    OptionalChainTemplate,
    OptionalMethodCannotBeAbstract,
    ParameterAfterRestParameter,
    PrivateDelete,
    PrivateNotInClass,
    PropertyAfterRestElement,
    RecordComputedPropertyUnsupported,
    RecordExtendsUnsupported,
    RecordInvalidPropertyName {
        name: String,
        static_: bool,
        method_: bool,
    },
    RecordPrivateElementUnsupported,
    RecordPropertyAnnotationRequired,
    Redeclaration(String, String),
    SetterArity,
    SetterMayNotHaveThisParam,
    StrictCatchVariable,
    StrictDelete,
    StrictDuplicateProperty,
    StrictFunctionName,
    StrictLHSAssignment,
    StrictLHSPostfix,
    StrictLHSPrefix,
    StrictModeWith,
    StrictNonOctalLiteral,
    StrictOctalLiteral,
    StrictParamDupe,
    StrictParamName,
    StrictParamNotSimple,
    StrictReservedWord,
    StrictVarName,
    StaticAbstractMethod,
    SuperPrivate,
    TSClassVisibility(TSClassVisibilityKind),
    ThisParamAnnotationRequired,
    ThisParamBannedInArrowFunctions,
    ThisParamBannedInConstructor,
    ThisParamBannedInConstructorType,
    ThisParamMayNotBeOptional,
    ThisParamMustBeFirst,
    TrailingCommaAfterRestElement,
    UnboundPrivate(String),
    Unexpected(String),
    UnexpectedTokenIllegal,
    UnexpectedEOS,
    UnexpectedExplicitInexactInObject,
    UnexpectedOpaqueTypeAlias,
    UnexpectedProto,
    UnexpectedReserved,
    UnexpectedReservedType,
    UnexpectedOptional,
    OptionalDestructuringMustHaveDefault,
    UnexpectedSpreadType,
    UnexpectedStatic,
    UnexpectedSuper,
    UnexpectedSuperCall,
    UnexpectedTokenWithSuggestion(&'static str, &'static str),
    UnexpectedTypeAlias,
    UnexpectedTypeAnnotation,
    UnexpectedTypeDeclaration,
    UnexpectedTypeExport,
    UnexpectedTypeImport,
    UnexpectedTypeInterface,
    UnexpectedVariance,
    UnexpectedWithExpected(String, String),
    UnknownLabel(String),
    UnsupportedDecorator,
    UnterminatedRegExp,
    WhitespaceInPrivateName,
    YieldAsIdentifierReference,
    YieldInFormalParameters,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AbstractMethodInNonAbstractClass => {
                write!(
                    f,
                    "Abstract methods can only appear within an abstract class."
                )
            }
            Self::AbstractMethodWithBody => {
                write!(f, "Abstract methods cannot have an implementation.")
            }
            Self::AbstractPropertyInNonAbstractClass => {
                write!(
                    f,
                    "Abstract properties can only appear within an abstract class."
                )
            }
            Self::AbstractPropertyWithInitializer => {
                write!(f, "Abstract properties cannot have an initializer.")
            }
            Self::AccessorDataProperty => {
                write!(
                    f,
                    "Object literal may not have data and accessor property with the same name"
                )
            }
            Self::AccessorGetSet => {
                write!(
                    f,
                    "Object literal may not have multiple get/set accessors with the same name"
                )
            }
            Self::AdjacentJSXElements => {
                write!(
                    f,
                    "Unexpected token <. Remember, adjacent JSX elements must be wrapped in an enclosing parent tag"
                )
            }
            Self::AmbiguousLetBracket => {
                write!(
                    f,
                    "`let [` is ambiguous in this position because it is either a `let` binding pattern, or a member expression."
                )
            }
            Self::AsyncFunctionAsStatement => {
                write!(
                    f,
                    "Async functions can only be declared at top level or immediately within another function."
                )
            }
            Self::AwaitAsIdentifierReference => {
                write!(f, "`await` is an invalid identifier in async functions")
            }
            Self::AwaitInAsyncFormalParameters => {
                write!(f, "`await` is not allowed in async function parameters.")
            }
            Self::ComputedShorthandProperty => {
                write!(f, "Computed properties must have a value.")
            }
            Self::ConstructorCannotBeAccessor => {
                write!(f, "Constructor can't be an accessor.")
            }
            Self::ConstructorCannotBeAsync => {
                write!(f, "Constructor can't be an async function.")
            }
            Self::ConstructorCannotBeGenerator => {
                write!(f, "Constructor can't be a generator.")
            }
            Self::ConstructorCannotBeOptional => {
                write!(f, "Constructor can't be optional.")
            }
            Self::DeclareAsync => {
                write!(
                    f,
                    "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type."
                )
            }
            Self::DeclareAsyncComponent => {
                write!(
                    f,
                    "async is an implementation detail and isn't necessary for declared components. Use `declare component` instead."
                )
            }
            Self::DeclareAsyncHook => {
                write!(
                    f,
                    "async is an implementation detail and isn't necessary for declared hooks. Use `declare hook` instead."
                )
            }
            Self::DeclareClassElement => {
                write!(f, "`declare` modifier can only appear on class fields.")
            }
            Self::DeclareClassFieldInitializer => {
                write!(
                    f,
                    "Unexpected token `=`. Initializers are not allowed in a `declare`."
                )
            }
            Self::DeclareOpaqueTypeInitializer => {
                write!(
                    f,
                    "Unexpected token `=`. Initializers are not allowed in a `declare opaque type`."
                )
            }
            Self::DuplicateConstructor => {
                write!(f, "Classes may only have one constructor")
            }
            Self::DuplicateExport(export) => {
                write!(f, "Duplicate export for `{}`", export)
            }
            Self::DuplicatePrivateFields(name) => {
                write!(
                    f,
                    "Private fields may only be declared once. `#{}` is declared more than once.",
                    name
                )
            }
            Self::ElementAfterRestElement => {
                write!(f, "Rest element must be final element of an array pattern")
            }
            Self::EnumBigIntMemberNotInitialized {
                enum_name,
                member_name,
            } => {
                write!(
                    f,
                    "bigint enum members need to be initialized, e.g. `{} = 1n,` in enum `{}`.",
                    member_name, enum_name
                )
            }
            Self::EnumBooleanMemberNotInitialized {
                enum_name,
                member_name,
            } => {
                write!(
                    f,
                    "Boolean enum members need to be initialized. Use either `{} = true,` or `{} = false,` in enum `{}`.",
                    member_name, member_name, enum_name
                )
            }
            Self::EnumDuplicateMemberName {
                enum_name,
                member_name,
            } => {
                write!(
                    f,
                    "Enum member names need to be unique, but the name `{}` has already been used before in enum `{}`.",
                    member_name, enum_name
                )
            }
            Self::EnumInconsistentMemberValues { enum_name } => {
                write!(
                    f,
                    "Enum `{}` has inconsistent member initializers. Either use no initializers, or consistently use literals (either booleans, numbers, or strings) for all member initializers.",
                    enum_name
                )
            }
            Self::EnumInvalidEllipsis { trailing_comma } => {
                if *trailing_comma {
                    write!(
                        f,
                        "The `...` must come at the end of the enum body. Remove the trailing comma."
                    )
                } else {
                    write!(
                        f,
                        "The `...` must come after all enum members. Move it to the end of the enum body."
                    )
                }
            }
            Self::EnumInvalidExplicitType {
                enum_name,
                supplied_type,
            } => {
                let suggestion = format!(
                    "Use one of `boolean`, `number`, `string`, `symbol`, or `bigint` in enum `{}`.",
                    enum_name
                );
                match supplied_type {
                    Some(supplied_type) => {
                        write!(
                            f,
                            "Enum type `{}` is not valid. {}",
                            supplied_type, suggestion
                        )
                    }
                    None => {
                        write!(f, "Supplied enum type is not valid. {}", suggestion)
                    }
                }
            }
            Self::EnumInvalidExport => {
                write!(
                    f,
                    "Cannot export an enum with `export type`, try `export enum E {{}}` or `module.exports = E;` instead."
                )
            }
            Self::EnumInvalidInitializerSeparator { member_name } => {
                write!(
                    f,
                    "Enum member names and initializers are separated with `=`. Replace `{}:` with `{} =`.",
                    member_name, member_name
                )
            }
            Self::EnumInvalidMemberInitializer {
                enum_name,
                explicit_type,
                member_name,
            } => match explicit_type.as_ref() {
                Some(explicit_type) => match explicit_type {
                    ExplicitType::Boolean
                    | ExplicitType::Number
                    | ExplicitType::String
                    | ExplicitType::BigInt => {
                        let explicit_type_str = explicit_type.as_str();
                        write!(
                            f,
                            "Enum `{}` has type `{}`, so the initializer of `{}` needs to be a {} literal.",
                            enum_name, explicit_type_str, member_name, explicit_type_str
                        )
                    }
                    ExplicitType::Symbol => {
                        write!(
                            f,
                            "Symbol enum members cannot be initialized. Use `{},` in enum `{}`.",
                            member_name, enum_name
                        )
                    }
                },
                None => {
                    write!(
                        f,
                        "The enum member initializer for `{}` needs to be a literal (either a boolean, number, or string) in enum `{}`.",
                        member_name, enum_name
                    )
                }
            },
            Self::EnumInvalidMemberSeparator => {
                write!(
                    f,
                    "Enum members are separated with `,`. Replace `;` with `,`."
                )
            }
            Self::EnumNumberMemberNotInitialized {
                enum_name,
                member_name,
            } => {
                write!(
                    f,
                    "Number enum members need to be initialized, e.g. `{} = 1,` in enum `{}`.",
                    member_name, enum_name
                )
            }
            Self::EnumStringMemberInconsistentlyInitialized { enum_name } => {
                write!(
                    f,
                    "String enum members need to consistently either all use initializers, or use no initializers, in enum {}.",
                    enum_name
                )
            }
            Self::ExpectedJSXClosingTag(name) => {
                write!(f, "Expected corresponding JSX closing tag for {}", name)
            }
            Self::ExpectedPatternFoundExpression => {
                write!(
                    f,
                    "Expected an object pattern, array pattern, or an identifier but found an expression instead"
                )
            }
            Self::ExportSpecifierMissingComma => {
                write!(f, "Missing comma between export specifiers")
            }
            Self::FunctionAsStatement { in_strict_mode } => {
                if *in_strict_mode {
                    write!(
                        f,
                        "In strict mode code, functions can only be declared at top level or immediately within another function."
                    )
                } else {
                    write!(
                        f,
                        "In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement."
                    )
                }
            }
            Self::GeneratorFunctionAsStatement => {
                write!(
                    f,
                    "Generators can only be declared at top level or immediately within another function."
                )
            }
            Self::GetterArity => {
                write!(f, "Getter should have zero parameters")
            }
            Self::GetterMayNotHaveThisParam => {
                write!(f, "A getter cannot have a `this` parameter.")
            }
            Self::IllegalBreak { in_match_statement } => {
                let extra = if *in_match_statement {
                    " `break` statements are not required in `match` statements, as unlike `switch` statements, `match` statement cases do not fall-through by default."
                } else {
                    ""
                };
                write!(f, "Illegal break statement.{}", extra)
            }
            Self::IllegalContinue => {
                write!(f, "Illegal continue statement")
            }
            Self::IllegalReturn => {
                write!(f, "Illegal return statement")
            }
            Self::IllegalUnicodeEscape => {
                write!(f, "Illegal Unicode escape")
            }
            Self::ImportAttributeMissingComma => {
                write!(f, "Missing comma between import attributes")
            }
            Self::ImportSpecifierMissingComma => {
                write!(f, "Missing comma between import specifiers")
            }
            Self::ImportTypeShorthandOnlyInPureImport => {
                write!(
                    f,
                    "The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. It cannot be used with `import type` or `import typeof` statements"
                )
            }
            Self::InexactInsideExact => {
                write!(
                    f,
                    "Explicit inexact syntax cannot appear inside an explicit exact object type"
                )
            }
            Self::InexactInsideNonObject => {
                write!(
                    f,
                    "Explicit inexact syntax can only appear inside an object type"
                )
            }
            Self::InvalidClassMemberName {
                name,
                static_,
                method_,
                private_,
            } => {
                let static_modifier = if *static_ { "static " } else { "" };
                let category = if *method_ { "methods" } else { "fields" };
                let name = if *private_ {
                    format!("#{}", name)
                } else {
                    name.clone()
                };
                write!(
                    f,
                    "Classes may not have {}{} named `{}`.",
                    static_modifier, category, name
                )
            }
            Self::InvalidComponentParamName => {
                write!(
                    f,
                    "Component params must be an identifier. If you'd like to destructure, you should use `name as {{destructure}}`"
                )
            }
            Self::InvalidComponentRenderAnnotation { .. } => {
                write!(
                    f,
                    "Components use `renders` instead of `:` to annotate the render type of a component."
                )
            }
            Self::InvalidComponentStringParameterBinding { optional, name } => {
                let camelized_name = camelize(name);
                let optional_suffix = if *optional { "?" } else { "" };
                write!(
                    f,
                    "String params require local bindings using `as` renaming. You can use `'{}' as {}{}: <TYPE>` ",
                    name, camelized_name, optional_suffix
                )
            }
            Self::InvalidFloatBigInt => {
                write!(f, "A bigint literal must be an integer")
            }
            Self::InvalidIndexedAccess { has_bracket } => {
                let msg = if *has_bracket {
                    "Remove the period."
                } else {
                    "Indexed access uses bracket notation."
                };
                write!(f, "Invalid indexed access. {} Use the format `T[K]`.", msg)
            }
            Self::InvalidJSXAttributeValue => {
                write!(
                    f,
                    "JSX value should be either an expression or a quoted JSX text"
                )
            }
            Self::InvalidLHSInAssignment => {
                write!(f, "Invalid left-hand side in assignment")
            }
            Self::InvalidLHSInExponentiation => {
                write!(f, "Invalid left-hand side in exponentiation expression")
            }
            Self::InvalidLHSInForIn => {
                write!(f, "Invalid left-hand side in for-in")
            }
            Self::InvalidLHSInForOf => {
                write!(f, "Invalid left-hand side in for-of")
            }
            Self::InvalidOptionalIndexedAccess => {
                write!(
                    f,
                    "Invalid optional indexed access. Indexed access uses bracket notation. Use the format `T?.[K]`."
                )
            }
            Self::InvalidRegExp => {
                write!(f, "Invalid regular expression")
            }
            Self::InvalidRegExpFlags(flags) => {
                write!(
                    f,
                    "Invalid flags supplied to RegExp constructor '{}'",
                    flags
                )
            }
            Self::InvalidSciBigInt => {
                write!(f, "A bigint literal cannot use exponential notation")
            }
            Self::InvalidTypeof => {
                write!(f, "`typeof` can only be used to get the type of variables.")
            }
            Self::InvalidTupleOptionalSpread => {
                write!(f, "Tuple spread elements cannot be optional.")
            }
            Self::InvalidTupleVariance => {
                write!(
                    f,
                    "Tuple variance annotations can only be used with labeled tuple elements, e.g. `[+foo: number]`"
                )
            }
            Self::JSXAttributeValueEmptyExpression => {
                write!(
                    f,
                    "JSX attributes must only be assigned a non-empty expression"
                )
            }
            Self::LiteralShorthandProperty => {
                write!(f, "Literals cannot be used as shorthand properties.")
            }
            Self::MalformedUnicode => {
                write!(f, "Malformed unicode")
            }
            Self::MatchNonLastRest(kind) => {
                let kind_str = match kind {
                    MatchNonLastRestKind::Object => "object",
                    MatchNonLastRestKind::Array => "array",
                };
                write!(
                    f,
                    "In match {} pattern, the rest must be the last element in the pattern",
                    kind_str
                )
            }
            Self::MatchEmptyArgument => {
                write!(f, "`match` argument must not be empty")
            }
            Self::MatchSpreadArgument => {
                write!(f, "`match` argument cannot contain spread elements")
            }
            Self::MatchExpressionAwait => {
                write!(f, "`await` is not yet supported in `match` expressions")
            }
            Self::MatchExpressionYield => {
                write!(f, "`yield` is not yet supported in `match` expressions")
            }
            Self::MethodInDestructuring => {
                write!(f, "Object pattern can't contain methods")
            }
            Self::MissingJSXClosingTag(name) => {
                write!(f, "JSX element {} has no corresponding closing tag.", name)
            }
            Self::MissingTypeParam => {
                write!(f, "Expected at least one type parameter.")
            }
            Self::MissingTypeParamDefault => {
                write!(
                    f,
                    "Type parameter declaration needs a default, since a preceding type parameter declaration has a default."
                )
            }
            Self::MultipleDefaultsInSwitch => {
                write!(f, "More than one default clause in switch statement")
            }
            Self::NewlineAfterThrow => {
                write!(f, "Illegal newline after throw")
            }
            Self::NewlineBeforeArrow => {
                write!(f, "Illegal newline before arrow")
            }
            Self::NoCatchOrFinally => {
                write!(f, "Missing catch or finally after try")
            }
            Self::NoUninitializedConst => {
                write!(f, "Const must be initialized")
            }
            Self::NoUninitializedDestructuring => {
                write!(f, "Destructuring assignment must be initialized")
            }
            Self::NullishCoalescingUnexpectedLogical(operator) => {
                write!(
                    f,
                    "Unexpected token `{}`. Parentheses are required to combine `??` with `&&` or `||` expressions.",
                    operator
                )
            }
            Self::OptionalChainNew => {
                write!(
                    f,
                    "An optional chain may not be used in a `new` expression."
                )
            }
            Self::OptionalChainTemplate => {
                write!(f, "Template literals may not be used in an optional chain.")
            }
            Self::OptionalMethodCannotBeAbstract => {
                write!(f, "Optional methods can't be abstract.")
            }
            Self::ParameterAfterRestParameter => {
                write!(
                    f,
                    "Rest parameter must be final parameter of an argument list"
                )
            }
            Self::PrivateDelete => {
                write!(f, "Private fields may not be deleted.")
            }
            Self::PrivateNotInClass => {
                write!(
                    f,
                    "Private fields can only be referenced from within a class."
                )
            }
            Self::PropertyAfterRestElement => {
                write!(
                    f,
                    "Rest property must be final property of an object pattern"
                )
            }
            Self::RecordComputedPropertyUnsupported => {
                write!(f, "Records do not support computed properties.")
            }
            Self::RecordExtendsUnsupported => {
                write!(
                    f,
                    "Records to not support `extends`: they do not allow hierarchies. Implementing an interface by using `implements` is supported."
                )
            }
            Self::RecordInvalidPropertyName {
                name,
                static_,
                method_,
            } => {
                let static_modifier = if *static_ { "static " } else { "" };
                let category = if *method_ { "methods " } else { "properties " };
                write!(
                    f,
                    "Records may not have {}{}named `{}`.",
                    static_modifier, category, name
                )
            }
            Self::RecordPrivateElementUnsupported => {
                write!(
                    f,
                    "Records to not support private elements. Remove the `#`."
                )
            }
            Self::RecordPropertyAnnotationRequired => {
                write!(f, "Record properties must have a type annotation.")
            }
            Self::Redeclaration(what, name) => {
                write!(f, "{} '{}' has already been declared", what, name)
            }
            Self::SetterArity => {
                write!(f, "Setter should have exactly one parameter")
            }
            Self::SetterMayNotHaveThisParam => {
                write!(f, "A setter cannot have a `this` parameter.")
            }
            Self::StrictCatchVariable => {
                write!(
                    f,
                    "Catch variable may not be eval or arguments in strict mode"
                )
            }
            Self::StrictDelete => {
                write!(f, "Delete of an unqualified identifier in strict mode.")
            }
            Self::StrictDuplicateProperty => {
                write!(
                    f,
                    "Duplicate data property in object literal not allowed in strict mode"
                )
            }
            Self::StrictFunctionName => {
                write!(
                    f,
                    "Function name may not be eval or arguments in strict mode"
                )
            }
            Self::StrictLHSAssignment => {
                write!(
                    f,
                    "Assignment to eval or arguments is not allowed in strict mode"
                )
            }
            Self::StrictLHSPostfix => {
                write!(
                    f,
                    "Postfix increment/decrement may not have eval or arguments operand in strict mode"
                )
            }
            Self::StrictLHSPrefix => {
                write!(
                    f,
                    "Prefix increment/decrement may not have eval or arguments operand in strict mode"
                )
            }
            Self::StrictModeWith => {
                write!(f, "Strict mode code may not include a with statement")
            }
            Self::StrictNonOctalLiteral => {
                write!(
                    f,
                    "Number literals with leading zeros are not allowed in strict mode."
                )
            }
            Self::StrictOctalLiteral => {
                write!(f, "Octal literals are not allowed in strict mode.")
            }
            Self::StrictParamDupe => {
                write!(
                    f,
                    "Strict mode function may not have duplicate parameter names"
                )
            }
            Self::StrictParamName => {
                write!(
                    f,
                    "Parameter name eval or arguments is not allowed in strict mode"
                )
            }
            Self::StrictParamNotSimple => {
                write!(
                    f,
                    "Illegal \"use strict\" directive in function with non-simple parameter list"
                )
            }
            Self::StrictReservedWord => {
                write!(f, "Use of reserved word in strict mode")
            }
            Self::StrictVarName => {
                write!(
                    f,
                    "Variable name may not be eval or arguments in strict mode"
                )
            }
            Self::SuperPrivate => {
                write!(
                    f,
                    "You may not access a private field through the `super` keyword."
                )
            }
            Self::StaticAbstractMethod => {
                write!(
                    f,
                    "`static` modifier can't be used with the `abstract` modifier."
                )
            }
            Self::TSClassVisibility(kind) => {
                let (keyword, append) = match kind {
                    TSClassVisibilityKind::Private => (
                        "private",
                        " You can try using JavaScript private fields by prepending `#` to the field name.",
                    ),
                    TSClassVisibilityKind::Public => (
                        "public",
                        " Fields and methods are public by default. You can simply omit the `public` keyword.",
                    ),
                    TSClassVisibilityKind::Protected => ("protected", ""),
                };
                write!(
                    f,
                    "Flow does not support using `{}` in classes.{}",
                    keyword, append
                )
            }
            Self::ThisParamAnnotationRequired => {
                write!(f, "A type annotation is required for the `this` parameter.")
            }
            Self::ThisParamBannedInArrowFunctions => {
                write!(
                    f,
                    "Arrow functions cannot have a `this` parameter; arrow functions automatically bind `this` when declared."
                )
            }
            Self::ThisParamBannedInConstructor => {
                write!(
                    f,
                    "Constructors cannot have a `this` parameter; constructors don't bind `this` like other functions."
                )
            }
            Self::ThisParamBannedInConstructorType => {
                write!(f, "Constructor types cannot have a `this` parameter.")
            }
            Self::ThisParamMayNotBeOptional => {
                write!(f, "The `this` parameter cannot be optional.")
            }
            Self::ThisParamMustBeFirst => {
                write!(
                    f,
                    "The `this` parameter must be the first function parameter."
                )
            }
            Self::TrailingCommaAfterRestElement => {
                write!(
                    f,
                    "A trailing comma is not permitted after the rest element"
                )
            }
            Self::UnboundPrivate(name) => {
                write!(
                    f,
                    "Private fields must be declared before they can be referenced. `#{}` has not been declared.",
                    name
                )
            }
            Self::Unexpected(unexpected) => {
                write!(f, "Unexpected {}", unexpected)
            }
            Self::UnexpectedTokenIllegal => {
                write!(f, "Unexpected token ILLEGAL")
            }
            Self::UnexpectedEOS => {
                write!(f, "Unexpected end of input")
            }
            Self::UnexpectedExplicitInexactInObject => {
                write!(
                    f,
                    "Explicit inexact syntax must come at the end of an object type"
                )
            }
            Self::UnexpectedOpaqueTypeAlias => {
                write!(f, "Opaque type aliases are not allowed in untyped mode")
            }
            Self::UnexpectedProto => {
                write!(f, "Unexpected proto modifier")
            }
            Self::UnexpectedReserved => {
                write!(f, "Unexpected reserved word")
            }
            Self::UnexpectedReservedType => {
                write!(f, "Unexpected reserved type")
            }
            Self::UnexpectedOptional => {
                write!(f, "Unexpected `?` (optional modifier not allowed here)")
            }
            Self::OptionalDestructuringMustHaveDefault => {
                write!(
                    f,
                    "Optional destructuring patterns must use a default value (e.g., `{{...}}: T = {{}}`)."
                )
            }
            Self::UnexpectedSpreadType => {
                write!(f, "Spreading a type is only allowed inside an object type")
            }
            Self::UnexpectedStatic => {
                write!(f, "Unexpected static modifier")
            }
            Self::UnexpectedSuper => {
                write!(f, "Unexpected `super` outside of a class method")
            }
            Self::UnexpectedSuperCall => {
                write!(f, "`super()` is only valid in a class constructor")
            }
            Self::UnexpectedTokenWithSuggestion(token, suggestion) => {
                write!(
                    f,
                    "Unexpected token `{}`. Did you mean `{}`?",
                    token, suggestion
                )
            }
            Self::UnexpectedTypeAlias => {
                write!(f, "Type aliases are not allowed in untyped mode")
            }
            Self::UnexpectedTypeAnnotation => {
                write!(f, "Type annotations are not allowed in untyped mode")
            }
            Self::UnexpectedTypeDeclaration => {
                write!(f, "Type declarations are not allowed in untyped mode")
            }
            Self::UnexpectedTypeExport => {
                write!(f, "Type exports are not allowed in untyped mode")
            }
            Self::UnexpectedTypeImport => {
                write!(f, "Type imports are not allowed in untyped mode")
            }
            Self::UnexpectedTypeInterface => {
                write!(f, "Interfaces are not allowed in untyped mode")
            }
            Self::UnexpectedVariance => {
                write!(f, "Unexpected variance sigil")
            }
            Self::UnexpectedWithExpected(unexpected, expected) => {
                write!(f, "Unexpected {}, expected {}", unexpected, expected)
            }
            Self::UnknownLabel(label) => {
                write!(f, "Undefined label '{}'", label)
            }
            Self::UnsupportedDecorator => {
                write!(f, "Found a decorator in an unsupported position.")
            }
            Self::UnterminatedRegExp => {
                write!(f, "Invalid regular expression: missing /")
            }
            Self::WhitespaceInPrivateName => {
                write!(f, "Unexpected whitespace between `#` and identifier")
            }
            Self::YieldAsIdentifierReference => {
                write!(f, "`yield` is an invalid identifier in generators")
            }
            Self::YieldInFormalParameters => {
                write!(f, "Yield expression not allowed in formal parameter")
            }
        }
    }
}

fn capitalize_ascii(s: &str) -> String {
    let mut chars: Vec<char> = s.chars().collect();
    if let Some(first_char) = chars.get_mut(0) {
        *first_char = first_char.to_ascii_uppercase();
    }
    chars.into_iter().collect()
}

pub fn camelize(str: &str) -> String {
    let parts: Vec<&str> = str.split('-').collect();
    if parts.is_empty() || parts.len() == 1 {
        return str.to_string();
    }
    let first = parts[0];
    let rest: Vec<String> = parts[1..].iter().map(|s| capitalize_ascii(s)).collect();
    let mut result = first.to_string();
    result.push_str(&rest.join(""));
    result
}
