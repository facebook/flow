(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | AccessorDataProperty
  | AccessorGetSet
  | AdjacentJSXElements
  | AmbiguousLetBracket
  | AsyncFunctionAsStatement
  | AwaitAsIdentifierReference
  | AwaitInAsyncFormalParameters
  | ComputedShorthandProperty
  | ConstructorCannotBeAccessor
  | ConstructorCannotBeAsync
  | ConstructorCannotBeGenerator
  | DeclareAsync
  | DeclareClassElement
  | DeclareClassFieldInitializer
  | DeclareOpaqueTypeInitializer
  | DuplicateConstructor
  | DuplicateExport of string
  | DuplicatePrivateFields of string
  | ElementAfterRestElement
  | EnumBigIntMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumBooleanMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumDuplicateMemberName of {
      enum_name: string;
      member_name: string;
    }
  | EnumInconsistentMemberValues of { enum_name: string }
  | EnumInvalidEllipsis of { trailing_comma: bool }
  | EnumInvalidExplicitType of {
      enum_name: string;
      supplied_type: string option;
    }
  | EnumInvalidExport
  | EnumInvalidInitializerSeparator of { member_name: string }
  | EnumInvalidMemberInitializer of {
      enum_name: string;
      explicit_type: Enum_common.explicit_type option;
      member_name: string;
    }
  | EnumInvalidMemberName of {
      enum_name: string;
      member_name: string;
    }
  | EnumInvalidMemberSeparator
  | EnumNumberMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumStringMemberInconsistentlyInitialized of { enum_name: string }
  | EnumInvalidConstPrefix
  | ExpectedJSXClosingTag of string
  | ExpectedPatternFoundExpression
  | ExportSpecifierMissingComma
  | FunctionAsStatement of { in_strict_mode: bool }
  | GeneratorFunctionAsStatement
  | GetterArity
  | GetterMayNotHaveThisParam
  | IllegalBreak of { in_match_statement: bool }
  | IllegalContinue
  | IllegalReturn
  | IllegalUnicodeEscape
  | ImportSpecifierMissingComma
  | ImportTypeShorthandOnlyInPureImport
  | InexactInsideExact
  | InexactInsideNonObject
  | InvalidClassMemberName of {
      name: string;
      static: bool;
      method_: bool;
      private_: bool;
    }
  | InvalidComponentParamName
  | InvalidComponentRenderAnnotation of { has_nested_render: bool }
  | InvalidComponentStringParameterBinding of {
      optional: bool;
      name: string;
    }
  | InvalidFloatBigInt
  | InvalidIndexedAccess of { has_bracket: bool }
  | InvalidJSXAttributeValue
  | InvalidLHSInAssignment
  | InvalidLHSInExponentiation
  | InvalidLHSInForIn
  | InvalidLHSInForOf
  | InvalidOptionalIndexedAccess
  | InvalidRegExp
  | InvalidRegExpFlags of string
  | InvalidSciBigInt
  | InvalidTupleOptionalSpread
  | InvalidTupleVariance
  | InvalidTypeof
  | JSXAttributeValueEmptyExpression
  | LiteralShorthandProperty
  | MalformedUnicode
  | MatchNonLastRest of [ `Object | `Array ]
  | MatchEmptyArgument
  | MatchSpreadArgument
  | MatchExpressionAwait
  | MatchExpressionYield
  | MethodInDestructuring
  | MissingJSXClosingTag of string
  | MissingTypeParam
  | MissingTypeParamDefault
  | MultipleDefaultsInSwitch
  | NewlineAfterThrow
  | NewlineBeforeArrow
  | NoCatchOrFinally
  | NoUninitializedConst
  | NoUninitializedDestructuring
  | NullishCoalescingUnexpectedLogical of string
  | OptionalChainNew
  | OptionalChainTemplate
  | ParameterAfterRestParameter
  | PrivateDelete
  | PrivateNotInClass
  | PropertyAfterRestElement
  | Redeclaration of string * string
  | SetterArity
  | SetterMayNotHaveThisParam
  | StrictCatchVariable
  | StrictDelete
  | StrictDuplicateProperty
  | StrictFunctionName
  | StrictLHSAssignment
  | StrictLHSPostfix
  | StrictLHSPrefix
  | StrictModeWith
  | StrictNonOctalLiteral
  | StrictOctalLiteral
  | StrictParamDupe
  | StrictParamName
  | StrictParamNotSimple
  | StrictReservedWord
  | StrictVarName
  | SuperPrivate
  | TSAbstractClass
  | TSClassVisibility of [ `Public | `Private | `Protected ]
  | TSTemplateLiteralType
  | ThisParamAnnotationRequired
  | ThisParamBannedInArrowFunctions
  | ThisParamBannedInConstructor
  | ThisParamMayNotBeOptional
  | ThisParamMustBeFirst
  | TrailingCommaAfterRestElement
  | UnboundPrivate of string
  | Unexpected of string
  | UnexpectedEOS
  | UnexpectedExplicitInexactInObject
  | UnexpectedOpaqueTypeAlias
  | UnexpectedProto
  | UnexpectedReserved
  | UnexpectedReservedType
  | UnexpectedSpreadType
  | UnexpectedStatic
  | UnexpectedSuper
  | UnexpectedSuperCall
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedTypeAlias
  | UnexpectedTypeAnnotation
  | UnexpectedTypeDeclaration
  | UnexpectedTypeExport
  | UnexpectedTypeImport
  | UnexpectedTypeInterface
  | UnexpectedVariance
  | UnexpectedWithExpected of string * string
  | UnknownLabel of string
  | UnsupportedDecorator
  | UnterminatedRegExp
  | WhitespaceInPrivateName
  | YieldAsIdentifierReference
  | YieldInFormalParameters
[@@deriving ord]

exception Error of (Loc.t * t) * (Loc.t * t) list

let error loc e = raise (Error ((loc, e), []))

module PP = struct
  let error = function
    | AccessorDataProperty ->
      "Object literal may not have data and accessor property with the same name"
    | AccessorGetSet -> "Object literal may not have multiple get/set accessors with the same name"
    | AdjacentJSXElements ->
      "Unexpected token <. Remember, adjacent JSX elements must be wrapped in an enclosing parent tag"
    | AmbiguousLetBracket ->
      "`let [` is ambiguous in this position because it is either a `let` binding pattern, or a member expression."
    | AsyncFunctionAsStatement ->
      "Async functions can only be declared at top level or immediately within another function."
    | AwaitAsIdentifierReference -> "`await` is an invalid identifier in async functions"
    | AwaitInAsyncFormalParameters -> "`await` is not allowed in async function parameters."
    | ComputedShorthandProperty -> "Computed properties must have a value."
    | ConstructorCannotBeAccessor -> "Constructor can't be an accessor."
    | ConstructorCannotBeAsync -> "Constructor can't be an async function."
    | ConstructorCannotBeGenerator -> "Constructor can't be a generator."
    | DeclareAsync ->
      "async is an implementation detail and isn't necessary for your declare function statement. "
      ^ "It is sufficient for your declare function to just have a Promise return type."
    | DeclareClassElement -> "`declare` modifier can only appear on class fields."
    | DeclareClassFieldInitializer ->
      "Unexpected token `=`. Initializers are not allowed in a `declare`."
    | DeclareOpaqueTypeInitializer ->
      "Unexpected token `=`. Initializers are not allowed in a `declare opaque type`."
    | DuplicateConstructor -> "Classes may only have one constructor"
    | DuplicateExport export -> Printf.sprintf "Duplicate export for `%s`" export
    | DuplicatePrivateFields name ->
      Printf.sprintf
        "Private fields may only be declared once. `#%s` is declared more than once."
        name
    | ElementAfterRestElement -> "Rest element must be final element of an array pattern"
    | EnumBigIntMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "bigint enum members need to be initialized, e.g. `%s = 1n,` in enum `%s`."
        member_name
        enum_name
    | EnumBooleanMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "Boolean enum members need to be initialized. Use either `%s = true,` or `%s = false,` in enum `%s`."
        member_name
        member_name
        enum_name
    | EnumDuplicateMemberName { enum_name; member_name } ->
      Printf.sprintf
        "Enum member names need to be unique, but the name `%s` has already been used before in enum `%s`."
        member_name
        enum_name
    | EnumInconsistentMemberValues { enum_name } ->
      Printf.sprintf
        "Enum `%s` has inconsistent member initializers. Either use no initializers, or consistently use literals (either booleans, numbers, or strings) for all member initializers."
        enum_name
    | EnumInvalidEllipsis { trailing_comma } ->
      if trailing_comma then
        "The `...` must come at the end of the enum body. Remove the trailing comma."
      else
        "The `...` must come after all enum members. Move it to the end of the enum body."
    | EnumInvalidExplicitType { enum_name; supplied_type } ->
      let suggestion =
        Printf.sprintf
          "Use one of `boolean`, `number`, `string`, `symbol`, or `bigint` in enum `%s`."
          enum_name
      in
      begin
        match supplied_type with
        | Some supplied_type ->
          Printf.sprintf "Enum type `%s` is not valid. %s" supplied_type suggestion
        | None -> Printf.sprintf "Supplied enum type is not valid. %s" suggestion
      end
    | EnumInvalidExport ->
      "Cannot export an enum with `export type`, try `export enum E {}` or `module.exports = E;` instead."
    | EnumInvalidInitializerSeparator { member_name } ->
      Printf.sprintf
        "Enum member names and initializers are separated with `=`. Replace `%s:` with `%s =`."
        member_name
        member_name
    | EnumInvalidMemberInitializer { enum_name; explicit_type; member_name } -> begin
      match explicit_type with
      | Some (Enum_common.Boolean as explicit_type)
      | Some (Enum_common.Number as explicit_type)
      | Some (Enum_common.String as explicit_type)
      | Some (Enum_common.BigInt as explicit_type) ->
        let explicit_type_str = Enum_common.string_of_explicit_type explicit_type in
        Printf.sprintf
          "Enum `%s` has type `%s`, so the initializer of `%s` needs to be a %s literal."
          enum_name
          explicit_type_str
          member_name
          explicit_type_str
      | Some Enum_common.Symbol ->
        Printf.sprintf
          "Symbol enum members cannot be initialized. Use `%s,` in enum `%s`."
          member_name
          enum_name
      | None ->
        Printf.sprintf
          "The enum member initializer for `%s` needs to be a literal (either a boolean, number, or string) in enum `%s`."
          member_name
          enum_name
    end
    | EnumInvalidMemberName { enum_name; member_name } ->
      (* Based on the error condition, we will only receive member names starting with [a-z] *)
      let suggestion = String.capitalize_ascii member_name in
      Printf.sprintf
        "Enum member names cannot start with lowercase 'a' through 'z'. Instead of using `%s`, consider using `%s`, in enum `%s`."
        member_name
        suggestion
        enum_name
    | EnumInvalidMemberSeparator -> "Enum members are separated with `,`. Replace `;` with `,`."
    | EnumNumberMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "Number enum members need to be initialized, e.g. `%s = 1,` in enum `%s`."
        member_name
        enum_name
    | EnumStringMemberInconsistentlyInitialized { enum_name } ->
      Printf.sprintf
        "String enum members need to consistently either all use initializers, or use no initializers, in enum %s."
        enum_name
    | EnumInvalidConstPrefix ->
      "`const` enums are not supported. Flow Enums are designed to allow for inlining, however the inlining itself needs to be part of the build system (whatever you use) rather than Flow itself."
    | ExpectedJSXClosingTag name ->
      Printf.sprintf "Expected corresponding JSX closing tag for %s" name
    | ExpectedPatternFoundExpression ->
      "Expected an object pattern, array pattern, or an identifier but found an expression instead"
    | ExportSpecifierMissingComma -> "Missing comma between export specifiers"
    | FunctionAsStatement { in_strict_mode } ->
      if in_strict_mode then
        "In strict mode code, functions can only be declared at top level or "
        ^ "immediately within another function."
      else
        "In non-strict mode code, functions can only be declared at top level, "
        ^ "inside a block, or as the body of an if statement."
    | GeneratorFunctionAsStatement ->
      "Generators can only be declared at top level or immediately within another function."
    | GetterArity -> "Getter should have zero parameters"
    | GetterMayNotHaveThisParam -> "A getter cannot have a `this` parameter."
    | IllegalBreak { in_match_statement } ->
      let extra =
        if in_match_statement then
          " `break` statements are not required in `match` statements, as unlike `switch` statements, `match` statement cases do not fall-through by default."
        else
          ""
      in
      Printf.sprintf "Illegal break statement.%s" extra
    | IllegalContinue -> "Illegal continue statement"
    | IllegalReturn -> "Illegal return statement"
    | IllegalUnicodeEscape -> "Illegal Unicode escape"
    | ImportSpecifierMissingComma -> "Missing comma between import specifiers"
    | ImportTypeShorthandOnlyInPureImport ->
      "The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. "
      ^ "It cannot be used with `import type` or `import typeof` statements"
    | InexactInsideExact ->
      "Explicit inexact syntax cannot appear inside an explicit exact object type"
    | InexactInsideNonObject -> "Explicit inexact syntax can only appear inside an object type"
    | InvalidClassMemberName { name; static; method_; private_ } ->
      let static_modifier =
        if static then
          "static "
        else
          ""
      in
      let category =
        if method_ then
          "methods"
        else
          "fields"
      in
      let name =
        if private_ then
          "#" ^ name
        else
          name
      in
      Printf.sprintf "Classes may not have %s%s named `%s`." static_modifier category name
    | InvalidComponentParamName ->
      "Component params must be an identifier. If you'd like to destructure, you should use `name as {destructure}`"
    | InvalidComponentRenderAnnotation _ ->
      "Components use `renders` instead of `:` to annotate the render type of a component."
    | InvalidComponentStringParameterBinding { optional; name } ->
      let camelized_name = Parse_error_utils.camelize name in
      Printf.sprintf
        "String params require local bindings using `as` renaming. You can use `'%s' as %s%s: <TYPE>` "
        name
        camelized_name
        ( if optional then
          "?"
        else
          ""
        )
    | InvalidFloatBigInt -> "A bigint literal must be an integer"
    | InvalidIndexedAccess { has_bracket } ->
      let msg =
        if has_bracket then
          "Remove the period."
        else
          "Indexed access uses bracket notation."
      in
      Printf.sprintf "Invalid indexed access. %s Use the format `T[K]`." msg
    | InvalidJSXAttributeValue -> "JSX value should be either an expression or a quoted JSX text"
    | InvalidLHSInAssignment -> "Invalid left-hand side in assignment"
    | InvalidLHSInExponentiation -> "Invalid left-hand side in exponentiation expression"
    | InvalidLHSInForIn -> "Invalid left-hand side in for-in"
    | InvalidLHSInForOf -> "Invalid left-hand side in for-of"
    | InvalidOptionalIndexedAccess ->
      "Invalid optional indexed access. Indexed access uses bracket notation. Use the format `T?.[K]`."
    | InvalidRegExp -> "Invalid regular expression"
    | InvalidRegExpFlags flags ->
      Printf.sprintf "Invalid flags supplied to RegExp constructor '%s'" flags
    | InvalidSciBigInt -> "A bigint literal cannot use exponential notation"
    | InvalidTypeof -> "`typeof` can only be used to get the type of variables."
    | InvalidTupleOptionalSpread -> "Tuple spread elements cannot be optional."
    | InvalidTupleVariance ->
      "Tuple variance annotations can only be used with labeled tuple elements, e.g. `[+foo: number]`"
    | JSXAttributeValueEmptyExpression ->
      "JSX attributes must only be assigned a non-empty expression"
    | LiteralShorthandProperty -> "Literals cannot be used as shorthand properties."
    | MalformedUnicode -> "Malformed unicode"
    | MatchNonLastRest kind ->
      let kind =
        match kind with
        | `Object -> "object"
        | `Array -> "array"
      in
      Printf.sprintf "In match %s pattern, the rest must be the last element in the pattern" kind
    | MatchEmptyArgument -> "`match` argument must not be empty"
    | MatchSpreadArgument -> "`match` argument cannot contain spread elements"
    | MatchExpressionAwait -> "`await` is not yet supported in `match` expressions"
    | MatchExpressionYield -> "`yield` is not yet supported in `match` expressions"
    | MethodInDestructuring -> "Object pattern can't contain methods"
    | MissingJSXClosingTag name ->
      Printf.sprintf "JSX element %s has no corresponding closing tag." name
    | MissingTypeParam -> "Expected at least one type parameter."
    | MissingTypeParamDefault ->
      "Type parameter declaration needs a default, since a preceding type parameter declaration has a default."
    | MultipleDefaultsInSwitch -> "More than one default clause in switch statement"
    | NewlineAfterThrow -> "Illegal newline after throw"
    | NewlineBeforeArrow -> "Illegal newline before arrow"
    | NoCatchOrFinally -> "Missing catch or finally after try"
    | NoUninitializedConst -> "Const must be initialized"
    | NoUninitializedDestructuring -> "Destructuring assignment must be initialized"
    | NullishCoalescingUnexpectedLogical operator ->
      Printf.sprintf
        "Unexpected token `%s`. Parentheses are required to combine `??` with `&&` or `||` expressions."
        operator
    | OptionalChainNew -> "An optional chain may not be used in a `new` expression."
    | OptionalChainTemplate -> "Template literals may not be used in an optional chain."
    | ParameterAfterRestParameter -> "Rest parameter must be final parameter of an argument list"
    | PrivateDelete -> "Private fields may not be deleted."
    | PrivateNotInClass -> "Private fields can only be referenced from within a class."
    | PropertyAfterRestElement -> "Rest property must be final property of an object pattern"
    | Redeclaration (what, name) -> Printf.sprintf "%s '%s' has already been declared" what name
    | SetterArity -> "Setter should have exactly one parameter"
    | SetterMayNotHaveThisParam -> "A setter cannot have a `this` parameter."
    | StrictCatchVariable -> "Catch variable may not be eval or arguments in strict mode"
    | StrictDelete -> "Delete of an unqualified identifier in strict mode."
    | StrictDuplicateProperty ->
      "Duplicate data property in object literal not allowed in strict mode"
    | StrictFunctionName -> "Function name may not be eval or arguments in strict mode"
    | StrictLHSAssignment -> "Assignment to eval or arguments is not allowed in strict mode"
    | StrictLHSPostfix ->
      "Postfix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictLHSPrefix ->
      "Prefix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictModeWith -> "Strict mode code may not include a with statement"
    | StrictNonOctalLiteral -> "Number literals with leading zeros are not allowed in strict mode."
    | StrictOctalLiteral -> "Octal literals are not allowed in strict mode."
    | StrictParamDupe -> "Strict mode function may not have duplicate parameter names"
    | StrictParamName -> "Parameter name eval or arguments is not allowed in strict mode"
    | StrictParamNotSimple ->
      "Illegal \"use strict\" directive in function with non-simple parameter list"
    | StrictReservedWord -> "Use of reserved word in strict mode"
    | StrictVarName -> "Variable name may not be eval or arguments in strict mode"
    | SuperPrivate -> "You may not access a private field through the `super` keyword."
    | TSAbstractClass -> "Flow does not support abstract classes."
    | TSClassVisibility kind ->
      let (keyword, append) =
        match kind with
        | `Private ->
          ( "private",
            " You can try using JavaScript private fields by prepending `#` to the field name."
          )
        | `Public ->
          ( "public",
            " Fields and methods are public by default. You can simply omit the `public` keyword."
          )
        | `Protected -> ("protected", "")
      in
      Printf.sprintf "Flow does not support using `%s` in classes.%s" keyword append
    | TSTemplateLiteralType -> "Flow does not support template literal types."
    | ThisParamAnnotationRequired -> "A type annotation is required for the `this` parameter."
    | ThisParamBannedInArrowFunctions ->
      "Arrow functions cannot have a `this` parameter; arrow functions automatically bind `this` when declared."
    | ThisParamBannedInConstructor ->
      "Constructors cannot have a `this` parameter; constructors don't bind `this` like other functions."
    | ThisParamMayNotBeOptional -> "The `this` parameter cannot be optional."
    | ThisParamMustBeFirst -> "The `this` parameter must be the first function parameter."
    | TrailingCommaAfterRestElement -> "A trailing comma is not permitted after the rest element"
    | UnboundPrivate name ->
      Printf.sprintf
        "Private fields must be declared before they can be referenced. `#%s` has not been declared."
        name
    | Unexpected unexpected -> Printf.sprintf "Unexpected %s" unexpected
    | UnexpectedEOS -> "Unexpected end of input"
    | UnexpectedExplicitInexactInObject ->
      "Explicit inexact syntax must come at the end of an object type"
    | UnexpectedOpaqueTypeAlias -> "Opaque type aliases are not allowed in untyped mode"
    | UnexpectedProto -> "Unexpected proto modifier"
    | UnexpectedReserved -> "Unexpected reserved word"
    | UnexpectedReservedType -> "Unexpected reserved type"
    | UnexpectedSpreadType -> "Spreading a type is only allowed inside an object type"
    | UnexpectedStatic -> "Unexpected static modifier"
    | UnexpectedSuper -> "Unexpected `super` outside of a class method"
    | UnexpectedSuperCall -> "`super()` is only valid in a class constructor"
    | UnexpectedTokenWithSuggestion (token, suggestion) ->
      Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?" token suggestion
    | UnexpectedTypeAlias -> "Type aliases are not allowed in untyped mode"
    | UnexpectedTypeAnnotation -> "Type annotations are not allowed in untyped mode"
    | UnexpectedTypeDeclaration -> "Type declarations are not allowed in untyped mode"
    | UnexpectedTypeExport -> "Type exports are not allowed in untyped mode"
    | UnexpectedTypeImport -> "Type imports are not allowed in untyped mode"
    | UnexpectedTypeInterface -> "Interfaces are not allowed in untyped mode"
    | UnexpectedVariance -> "Unexpected variance sigil"
    | UnexpectedWithExpected (unexpected, expected) ->
      Printf.sprintf "Unexpected %s, expected %s" unexpected expected
    | UnknownLabel label -> Printf.sprintf "Undefined label '%s'" label
    | UnsupportedDecorator -> "Found a decorator in an unsupported position."
    | UnterminatedRegExp -> "Invalid regular expression: missing /"
    | WhitespaceInPrivateName -> "Unexpected whitespace between `#` and identifier"
    | YieldAsIdentifierReference -> "`yield` is an invalid identifier in generators"
    | YieldInFormalParameters -> "Yield expression not allowed in formal parameter"
end
