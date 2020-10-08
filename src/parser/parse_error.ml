(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Assertion of string
  | EnumBooleanMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumDuplicateMemberName of {
      enum_name: string;
      member_name: string;
    }
  | EnumInconsistentMemberValues of { enum_name: string }
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
  | EnumInvalidEllipsis of { trailing_comma: bool }
  | EnumNumberMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumStringMemberInconsistentlyInitailized of { enum_name: string }
  | Unexpected of string
  | UnexpectedWithExpected of string * string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedReserved
  | UnexpectedReservedType
  | UnexpectedSuper
  | UnexpectedSuperCall
  | UnexpectedEOS
  | UnexpectedVariance
  | UnexpectedStatic
  | UnexpectedProto
  | UnexpectedTypeAlias
  | UnexpectedOpaqueTypeAlias
  | UnexpectedTypeAnnotation
  | UnexpectedTypeDeclaration
  | UnexpectedTypeImport
  | UnexpectedTypeExport
  | UnexpectedTypeInterface
  | UnexpectedSpreadType
  | UnexpectedExplicitInexactInObject
  | InexactInsideExact
  | InexactInsideNonObject
  | NewlineAfterThrow
  | InvalidFloatBigInt
  | InvalidSciBigInt
  | InvalidRegExp
  | InvalidRegExpFlags of string
  | UnterminatedRegExp
  | InvalidLHSInAssignment
  | InvalidLHSInExponentiation
  | InvalidLHSInForIn
  | InvalidLHSInForOf
  | ExpectedPatternFoundExpression
  | MultipleDefaultsInSwitch
  | NoCatchOrFinally
  | UnknownLabel of string
  | Redeclaration of string * string
  | IllegalContinue
  | IllegalBreak
  | IllegalReturn
  | IllegalUnicodeEscape
  | StrictModeWith
  | StrictCatchVariable
  | StrictVarName
  | StrictParamName
  | StrictParamDupe
  | StrictFunctionName
  | StrictOctalLiteral
  | StrictNonOctalLiteral
  | StrictDelete
  | StrictDuplicateProperty
  | AccessorDataProperty
  | AccessorGetSet
  | StrictLHSAssignment
  | StrictLHSPostfix
  | StrictLHSPrefix
  | StrictReservedWord
  | JSXAttributeValueEmptyExpression
  | InvalidJSXAttributeValue
  | ExpectedJSXClosingTag of string
  | NoUninitializedConst
  | NoUninitializedDestructuring
  | NewlineBeforeArrow
  | FunctionAsStatement of { in_strict_mode: bool }
  | AsyncFunctionAsStatement
  | GeneratorFunctionAsStatement
  | AdjacentJSXElements
  | ParameterAfterRestParameter
  | ElementAfterRestElement
  | PropertyAfterRestElement
  | DeclareAsync
  | DeclareClassElement
  | DeclareClassFieldInitializer
  | DeclareExportLet
  | DeclareExportConst
  | DeclareExportType
  | DeclareExportInterface
  | UnexpectedExportStarAs
  | DuplicateExport of string
  | ExportNamelessClass
  | ExportNamelessFunction
  | UnsupportedDecorator
  | MissingTypeParamDefault
  | DuplicateDeclareModuleExports
  | AmbiguousDeclareModuleKind
  | GetterArity
  | SetterArity
  | InvalidNonTypeImportInDeclareModule
  | ImportTypeShorthandOnlyInPureImport
  | ImportSpecifierMissingComma
  | ExportSpecifierMissingComma
  | MalformedUnicode
  | DuplicateConstructor
  | DuplicatePrivateFields of string
  | InvalidFieldName of {
      name: string;
      static: bool;
      private_: bool;
    }
  | PrivateMethod
  | PrivateDelete
  | UnboundPrivate of string
  | PrivateNotInClass
  | SuperPrivate
  | YieldInFormalParameters
  | AwaitAsIdentifierReference
  | YieldAsIdentifierReference
  | AmbiguousLetBracket
  | LiteralShorthandProperty
  | ComputedShorthandProperty
  | MethodInDestructuring
  | TrailingCommaAfterRestElement
  | OptionalChainingDisabled
  | OptionalChainNew
  | OptionalChainTemplate
  | NullishCoalescingDisabled
  | NullishCoalescingUnexpectedLogical of string
  | WhitespaceInPrivateName
[@@deriving ord]

exception Error of (Loc.t * t) list

let error loc e = raise (Error [(loc, e)])

module PP = struct
  let error = function
    | Assertion str -> "Unexpected parser state: " ^ str
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
    | EnumInvalidExplicitType { enum_name; supplied_type } ->
      let suggestion =
        Printf.sprintf
          "Use one of `boolean`, `number`, `string`, or `symbol` in enum `%s`."
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
    | EnumInvalidMemberInitializer { enum_name; explicit_type; member_name } ->
      begin
        match explicit_type with
        | Some (Enum_common.Boolean as explicit_type)
        | Some (Enum_common.Number as explicit_type)
        | Some (Enum_common.String as explicit_type) ->
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
    | EnumInvalidEllipsis { trailing_comma } ->
      if trailing_comma then
        "The `...` must come at the end of the enum body. Remove the trailing comma."
      else
        "The `...` must come after all enum members. Move it to the end of the enum body."
    | EnumNumberMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "Number enum members need to be initialized, e.g. `%s = 1,` in enum `%s`."
        member_name
        enum_name
    | EnumStringMemberInconsistentlyInitailized { enum_name } ->
      Printf.sprintf
        "String enum members need to consistently either all use initializers, or use no initializers, in enum %s."
        enum_name
    | Unexpected unexpected -> Printf.sprintf "Unexpected %s" unexpected
    | UnexpectedWithExpected (unexpected, expected) ->
      Printf.sprintf "Unexpected %s, expected %s" unexpected expected
    | UnexpectedTokenWithSuggestion (token, suggestion) ->
      Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?" token suggestion
    | UnexpectedReserved -> "Unexpected reserved word"
    | UnexpectedReservedType -> "Unexpected reserved type"
    | UnexpectedSuper -> "Unexpected `super` outside of a class method"
    | UnexpectedSuperCall -> "`super()` is only valid in a class constructor"
    | UnexpectedEOS -> "Unexpected end of input"
    | UnexpectedVariance -> "Unexpected variance sigil"
    | UnexpectedStatic -> "Unexpected static modifier"
    | UnexpectedProto -> "Unexpected proto modifier"
    | UnexpectedTypeAlias -> "Type aliases are not allowed in untyped mode"
    | UnexpectedOpaqueTypeAlias -> "Opaque type aliases are not allowed in untyped mode"
    | UnexpectedTypeAnnotation -> "Type annotations are not allowed in untyped mode"
    | UnexpectedTypeDeclaration -> "Type declarations are not allowed in untyped mode"
    | UnexpectedTypeImport -> "Type imports are not allowed in untyped mode"
    | UnexpectedTypeExport -> "Type exports are not allowed in untyped mode"
    | UnexpectedTypeInterface -> "Interfaces are not allowed in untyped mode"
    | UnexpectedSpreadType -> "Spreading a type is only allowed inside an object type"
    | UnexpectedExplicitInexactInObject ->
      "Explicit inexact syntax must come at the end of an object type"
    | InexactInsideExact ->
      "Explicit inexact syntax cannot appear inside an explicit exact object type"
    | InexactInsideNonObject -> "Explicit inexact syntax can only appear inside an object type"
    | NewlineAfterThrow -> "Illegal newline after throw"
    | InvalidFloatBigInt -> "A bigint literal must be an integer"
    | InvalidSciBigInt -> "A bigint literal cannot use exponential notation"
    | InvalidRegExp -> "Invalid regular expression"
    | InvalidRegExpFlags flags -> "Invalid flags supplied to RegExp constructor '" ^ flags ^ "'"
    | UnterminatedRegExp -> "Invalid regular expression: missing /"
    | InvalidLHSInAssignment -> "Invalid left-hand side in assignment"
    | InvalidLHSInExponentiation -> "Invalid left-hand side in exponentiation expression"
    | InvalidLHSInForIn -> "Invalid left-hand side in for-in"
    | InvalidLHSInForOf -> "Invalid left-hand side in for-of"
    | ExpectedPatternFoundExpression ->
      "Expected an object pattern, array pattern, or an identifier but "
      ^ "found an expression instead"
    | MultipleDefaultsInSwitch -> "More than one default clause in switch statement"
    | NoCatchOrFinally -> "Missing catch or finally after try"
    | UnknownLabel label -> "Undefined label '" ^ label ^ "'"
    | Redeclaration (what, name) -> what ^ " '" ^ name ^ "' has already been declared"
    | IllegalContinue -> "Illegal continue statement"
    | IllegalBreak -> "Illegal break statement"
    | IllegalReturn -> "Illegal return statement"
    | IllegalUnicodeEscape -> "Illegal Unicode escape"
    | StrictModeWith -> "Strict mode code may not include a with statement"
    | StrictCatchVariable -> "Catch variable may not be eval or arguments in strict mode"
    | StrictVarName -> "Variable name may not be eval or arguments in strict mode"
    | StrictParamName -> "Parameter name eval or arguments is not allowed in strict mode"
    | StrictParamDupe -> "Strict mode function may not have duplicate parameter names"
    | StrictFunctionName -> "Function name may not be eval or arguments in strict mode"
    | StrictOctalLiteral -> "Octal literals are not allowed in strict mode."
    | StrictNonOctalLiteral -> "Number literals with leading zeros are not allowed in strict mode."
    | StrictDelete -> "Delete of an unqualified identifier in strict mode."
    | StrictDuplicateProperty ->
      "Duplicate data property in object literal not allowed in strict mode"
    | AccessorDataProperty ->
      "Object literal may not have data and accessor property with the same name"
    | AccessorGetSet -> "Object literal may not have multiple get/set accessors with the same name"
    | StrictLHSAssignment -> "Assignment to eval or arguments is not allowed in strict mode"
    | StrictLHSPostfix ->
      "Postfix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictLHSPrefix ->
      "Prefix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictReservedWord -> "Use of future reserved word in strict mode"
    | JSXAttributeValueEmptyExpression ->
      "JSX attributes must only be assigned a non-empty expression"
    | InvalidJSXAttributeValue -> "JSX value should be either an expression or a quoted JSX text"
    | ExpectedJSXClosingTag name -> "Expected corresponding JSX closing tag for " ^ name
    | NoUninitializedConst -> "Const must be initialized"
    | NoUninitializedDestructuring -> "Destructuring assignment must be initialized"
    | NewlineBeforeArrow -> "Illegal newline before arrow"
    | FunctionAsStatement { in_strict_mode } ->
      if in_strict_mode then
        "In strict mode code, functions can only be declared at top level or "
        ^ "immediately within another function."
      else
        "In non-strict mode code, functions can only be declared at top level, "
        ^ "inside a block, or as the body of an if statement."
    | AsyncFunctionAsStatement ->
      "Async functions can only be declared at top level or "
      ^ "immediately within another function."
    | GeneratorFunctionAsStatement ->
      "Generators can only be declared at top level or " ^ "immediately within another function."
    | AdjacentJSXElements ->
      "Unexpected token <. Remember, adjacent JSX "
      ^ "elements must be wrapped in an enclosing parent tag"
    | ParameterAfterRestParameter -> "Rest parameter must be final parameter of an argument list"
    | ElementAfterRestElement -> "Rest element must be final element of an array pattern"
    | PropertyAfterRestElement -> "Rest property must be final property of an object pattern"
    | DeclareAsync ->
      "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type."
    | DeclareClassElement -> "`declare` modifier can only appear on class fields."
    | DeclareClassFieldInitializer -> "Initializers are not allowed in a `declare`."
    | DeclareExportLet -> "`declare export let` is not supported. Use `declare export var` instead."
    | DeclareExportConst ->
      "`declare export const` is not supported. Use `declare export var` instead."
    | DeclareExportType -> "`declare export type` is not supported. Use `export type` instead."
    | DeclareExportInterface ->
      "`declare export interface` is not supported. Use `export interface` instead."
    | UnexpectedExportStarAs ->
      "`export * as` is an early-stage proposal and is not enabled by default. To enable support in the parser, use the `esproposal_export_star_as` option"
    | DuplicateExport export -> Printf.sprintf "Duplicate export for `%s`" export
    | ExportNamelessClass ->
      "When exporting a class as a named export, you must specify a class name. Did you mean `export default class ...`?"
    | ExportNamelessFunction ->
      "When exporting a function as a named export, you must specify a function name. Did you mean `export default function ...`?"
    | UnsupportedDecorator -> "Found a decorator in an unsupported position."
    | MissingTypeParamDefault ->
      "Type parameter declaration needs a default, since a preceding type parameter declaration has a default."
    | DuplicateDeclareModuleExports -> "Duplicate `declare module.exports` statement!"
    | AmbiguousDeclareModuleKind ->
      "Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module."
    | GetterArity -> "Getter should have zero parameters"
    | SetterArity -> "Setter should have exactly one parameter"
    | InvalidNonTypeImportInDeclareModule ->
      "Imports within a `declare module` body must always be " ^ "`import type` or `import typeof`!"
    | ImportTypeShorthandOnlyInPureImport ->
      "The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. It cannot be used with `import type` or `import typeof` statements"
    | ImportSpecifierMissingComma -> "Missing comma between import specifiers"
    | ExportSpecifierMissingComma -> "Missing comma between export specifiers"
    | MalformedUnicode -> "Malformed unicode"
    | DuplicateConstructor -> "Classes may only have one constructor"
    | DuplicatePrivateFields name ->
      "Private fields may only be declared once. `#" ^ name ^ "` is declared more than once."
    | InvalidFieldName { name; static; private_ } ->
      let static_modifier =
        if static then
          "static "
        else
          ""
      in
      let name =
        if private_ then
          "#" ^ name
        else
          name
      in
      "Classes may not have " ^ static_modifier ^ "fields named `" ^ name ^ "`."
    | PrivateMethod -> "Classes may not have private methods."
    | PrivateDelete -> "Private fields may not be deleted."
    | UnboundPrivate name ->
      "Private fields must be declared before they can be referenced. `#"
      ^ name
      ^ "` has not been declared."
    | PrivateNotInClass -> "Private fields can only be referenced from within a class."
    | SuperPrivate -> "You may not access a private field through the `super` keyword."
    | YieldInFormalParameters -> "Yield expression not allowed in formal parameter"
    | AwaitAsIdentifierReference -> "`await` is an invalid identifier in async functions"
    | YieldAsIdentifierReference -> "`yield` is an invalid identifier in generators"
    | AmbiguousLetBracket ->
      "`let [` is ambiguous in this position because it is "
      ^ "either a `let` binding pattern, or a member expression."
    | LiteralShorthandProperty -> "Literals cannot be used as shorthand properties."
    | ComputedShorthandProperty -> "Computed properties must have a value."
    | MethodInDestructuring -> "Object pattern can't contain methods"
    | TrailingCommaAfterRestElement -> "A trailing comma is not permitted after the rest element"
    | OptionalChainingDisabled ->
      "The optional chaining plugin must be enabled in order to use the optional chaining operator (`?.`). Optional chaining is an active early-stage feature proposal which may change and is not enabled by default. To enable support in the parser, use the `esproposal_optional_chaining` option."
    | OptionalChainNew -> "An optional chain may not be used in a `new` expression."
    | OptionalChainTemplate -> "Template literals may not be used in an optional chain."
    | NullishCoalescingDisabled ->
      "The nullish coalescing plugin must be enabled in order to use the nullish coalescing operator (`??`). Nullish coalescing is an active early-stage feature proposal which may change and is not enabled by default. To enable support in the parser, use the `esproposal_nullish_coalescing` option."
    | NullishCoalescingUnexpectedLogical operator ->
      Printf.sprintf
        "Unexpected token `%s`. Parentheses are required to combine `??` with `&&` or `||` expressions."
        operator
    | WhitespaceInPrivateName -> "Unexpected whitespace between `#` and identifier"
end
