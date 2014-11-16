(**
 *  Copyright 2012-2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

module Ast = Spider_monkey_ast

type t =
  | UnexpectedToken of string
  | UnexpectedNumber
  | UnexpectedString
  | UnexpectedIdentifier
  | UnexpectedReserved
  | UnexpectedEOS
  | NewlineAfterThrow
  | InvalidRegExp
  | InvalidRegExpFlags of string
  | UnterminatedRegExp
  | InvalidLHSInAssignment
  | InvalidLHSInForIn
  | MultipleDefaultsInSwitch
  | NoCatchOrFinally
  | UnknownLabel of string
  | Redeclaration of string * string
  | IllegalContinue
  | IllegalBreak
  | IllegalReturn
  | IllegalYield
  | StrictModeWith
  | StrictCatchVariable
  | StrictVarName
  | StrictParamName
  | StrictParamDupe
  | StrictFunctionName
  | StrictOctalLiteral
  | StrictDelete
  | StrictDuplicateProperty
  | AccessorDataProperty
  | AccessorGetSet
  | StrictLHSAssignment
  | StrictLHSPostfix
  | StrictLHSPrefix
  | StrictReservedWord
  | XJSAttributeValueEmptyExpression
  | InvalidXJSAttributeValue
  | ExpectedXJSClosingTag of string
  | NoUnintializedConst
  | NewlineBeforeArrow
  | StrictFunctionStatement
  | AdjacentXJSElements

exception Error of (Ast.Loc.t * t) list

let error loc e =
  raise (Error [loc, e])

module PP =
  struct
    let error = function
      | UnexpectedToken token->  "Unexpected token "^token
      | UnexpectedNumber ->  "Unexpected number"
      | UnexpectedString ->  "Unexpected string"
      | UnexpectedIdentifier ->  "Unexpected identifier"
      | UnexpectedReserved ->  "Unexpected reserved word"
      | UnexpectedEOS ->  "Unexpected end of input"
      | NewlineAfterThrow ->  "Illegal newline after throw"
      | InvalidRegExp -> "Invalid regular expression"
      | InvalidRegExpFlags flags -> "Invalid flags supplied to RegExp constructor '"^flags^"'"
      | UnterminatedRegExp ->  "Invalid regular expression: missing /"
      | InvalidLHSInAssignment ->  "Invalid left-hand side in assignment"
      | InvalidLHSInForIn ->  "Invalid left-hand side in for-in"
      | MultipleDefaultsInSwitch -> "More than one default clause in switch statement"
      | NoCatchOrFinally ->  "Missing catch or finally after try"
      | UnknownLabel label -> "Undefined label '"^label^"'"
      | Redeclaration (what, name)-> what^" '"^name^"' has already been declared"
      | IllegalContinue -> "Illegal continue statement"
      | IllegalBreak -> "Illegal break statement"
      | IllegalReturn -> "Illegal return statement"
      | IllegalYield -> "Illegal yield expression"
      | StrictModeWith ->  "Strict mode code may not include a with statement"
      | StrictCatchVariable ->  "Catch variable may not be eval or arguments in strict mode"
      | StrictVarName ->  "Variable name may not be eval or arguments in strict mode"
      | StrictParamName ->  "Parameter name eval or arguments is not allowed in strict mode"
      | StrictParamDupe -> "Strict mode function may not have duplicate parameter names"
      | StrictFunctionName ->  "Function name may not be eval or arguments in strict mode"
      | StrictOctalLiteral ->  "Octal literals are not allowed in strict mode."
      | StrictDelete ->  "Delete of an unqualified identifier in strict mode."
      | StrictDuplicateProperty ->  "Duplicate data property in object literal not allowed in strict mode"
      | AccessorDataProperty ->  "Object literal may not have data and accessor property with the same name"
      | AccessorGetSet ->  "Object literal may not have multiple get/set accessors with the same name"
      | StrictLHSAssignment ->  "Assignment to eval or arguments is not allowed in strict mode"
      | StrictLHSPostfix ->  "Postfix increment/decrement may not have eval or arguments operand in strict mode"
      | StrictLHSPrefix ->  "Prefix increment/decrement may not have eval or arguments operand in strict mode"
      | StrictReservedWord ->  "Use of future reserved word in strict mode"
      | XJSAttributeValueEmptyExpression -> "XJS attributes must only be assigned a non-empty expression"
      | InvalidXJSAttributeValue -> "XJS value should be either an expression or a quoted XJS text"
      | ExpectedXJSClosingTag name -> "Expected corresponding XJS closing tag for "^name
      | NoUnintializedConst -> "Const must be initialized"
      | NewlineBeforeArrow ->  "Illegal newline before arrow"
      | StrictFunctionStatement -> "In strict mode code, functions can only be"^
          " declared at top level or immediately within another function."
      | AdjacentXJSElements -> "Unexpected token <. Remember, adjacent XJS "^
          "elements must be wrapped in an enclosing parent tag"
  end
