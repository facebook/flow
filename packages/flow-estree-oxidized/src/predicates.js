/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

'use strict';

/*::
import type {
  ESNode,
  Token,
  MostTokens,
  BlockComment,
  LineComment,
  AFunction,
  PropertyDefinition,
  PropertyDefinitionWithNonComputedName,
  MethodDefinition,
  MethodDefinitionConstructor,
  MethodDefinitionWithNonComputedName,
  MemberExpression,
  MemberExpressionWithNonComputedName,
  ObjectPropertyWithShorthandStaticName,
  ObjectPropertyWithNonShorthandStaticName,
  DestructuringObjectPropertyWithShorthandStaticName,
  DestructuringObjectPropertyWithNonShorthandStaticName,
  ClassMember,
  ClassDeclaration,
  ClassExpression,
  Literal,
  BigIntLiteral,
  BooleanLiteral,
  NullLiteral,
  NumericLiteral,
  RegExpLiteral,
  StringLiteral,
  Identifier,
  EnumDefaultedMember,
  Expression,
  Statement,
} from './types';
*/

export * from './generated/predicates';

export function isClass(node /*: ESNode */) /*: implies node is (ClassDeclaration | ClassExpression) */ {
  return node.type === 'ClassDeclaration' || node.type === 'ClassExpression';
}

export function isPropertyDefinitionWithNonComputedName(
  node /*: ESNode */,
) /*: implies node is PropertyDefinitionWithNonComputedName */ {
  return node.type === 'PropertyDefinition' && node.computed === false;
}

export function isClassMember(node /*: ESNode */) /*: implies node is ClassMember */ {
  return node.type === 'PropertyDefinition' || node.type === 'MethodDefinition';
}

export function isClassMemberWithNonComputedName(
  node /*: ESNode */,
) /*: implies node is (PropertyDefinitionWithNonComputedName | MethodDefinitionConstructor | MethodDefinitionWithNonComputedName) */ {
  return (node.type === 'PropertyDefinition' || node.type === 'MethodDefinition') && node.computed === false;
}

export function isComment(node /*: ESNode | Token */) /*: implies node is (MostTokens | BlockComment | LineComment) */ {
  return node.type === 'Block' || node.type === 'Line';
}

export function isFunction(node /*: ESNode */) /*: implies node is AFunction */ {
  return (
    node.type === 'ArrowFunctionExpression' ||
    node.type === 'FunctionDeclaration' ||
    node.type === 'FunctionExpression'
  );
}

export function isMethodDefinitionWithNonComputedName(
  node /*: ESNode */,
) /*: implies node is (MethodDefinitionConstructor | MethodDefinitionWithNonComputedName) */ {
  return node.type === 'MethodDefinition' && node.computed === false;
}

export function isMemberExpressionWithNonComputedProperty(
  node /*: ESNode */,
) /*: implies node is MemberExpressionWithNonComputedName */ {
  return node.type === 'MemberExpression' && node.computed === false;
}

export function isOptionalMemberExpressionWithNonComputedProperty(
  node /*: ESNode */,
) /*: implies node is MemberExpressionWithNonComputedName */ {
  return node.type === 'MemberExpression' && node.computed === false;
}

export function isObjectPropertyWithShorthand(node /*: ESNode */) /*: implies node is (ObjectPropertyWithShorthandStaticName | DestructuringObjectPropertyWithShorthandStaticName) */ {
  return node.type === 'Property' && node.shorthand === true;
}

export function isObjectPropertyWithNonComputedName(node /*: ESNode */) /*: implies node is (ObjectPropertyWithNonShorthandStaticName | ObjectPropertyWithShorthandStaticName | DestructuringObjectPropertyWithNonShorthandStaticName | DestructuringObjectPropertyWithShorthandStaticName) */ {
  return node.type === 'Property' && node.computed === false;
}

export function isBigIntLiteral(node /*: ESNode */) /*: implies node is BigIntLiteral */ {
  return node.type === 'Literal' && node.literalType === 'bigint';
}

export function isBooleanLiteral(node /*: ESNode */) /*: implies node is BooleanLiteral */ {
  return node.type === 'Literal' && node.literalType === 'boolean';
}

export function isNullLiteral(node /*: ESNode */) /*: implies node is NullLiteral */ {
  return node.type === 'Literal' && node.literalType === 'null';
}

export function isNumericLiteral(node /*: ESNode */) /*: implies node is NumericLiteral */ {
  return node.type === 'Literal' && node.literalType === 'numeric';
}

export function isRegExpLiteral(node /*: ESNode */) /*: implies node is RegExpLiteral */ {
  return node.type === 'Literal' &&  node.literalType === 'regexp';
}

export function isStringLiteral(node /*: ESNode */) /*: implies node is StringLiteral */ {
  return node.type === 'Literal' && node.literalType === 'string';
}

export function isExpression(node /*: ESNode */) /*: implies node is Expression */ {
  return (
    node.type === 'ThisExpression' ||
    node.type === 'ArrayExpression' ||
    node.type === 'ObjectExpression' ||
    node.type === 'FunctionExpression' ||
    node.type === 'ArrowFunctionExpression' ||
    node.type === 'YieldExpression' ||
    node.type === 'Literal' ||
    node.type === 'UnaryExpression' ||
    node.type === 'UpdateExpression' ||
    node.type === 'BinaryExpression' ||
    node.type === 'AssignmentExpression' ||
    node.type === 'LogicalExpression' ||
    node.type === 'MemberExpression' ||
    node.type === 'ConditionalExpression' ||
    node.type === 'CallExpression' ||
    node.type === 'NewExpression' ||
    node.type === 'SequenceExpression' ||
    node.type === 'TemplateLiteral' ||
    node.type === 'TaggedTemplateExpression' ||
    node.type === 'ClassExpression' ||
    node.type === 'MetaProperty' ||
    node.type === 'Identifier' ||
    node.type === 'AwaitExpression' ||
    node.type === 'ImportExpression' ||
    node.type === 'ChainExpression' ||
    node.type === 'TypeCastExpression' ||
    node.type === 'AsExpression' ||
    node.type === 'AsConstExpression' ||
    node.type === 'JSXFragment' ||
    node.type === 'JSXElement' ||
    node.type === 'MatchExpression' ||
    node.type === 'RecordExpression'
  );
}

export function isStatement(node /*: ESNode */) /*: implies node is Statement */ {
  return (
    node.type === 'BlockStatement' ||
    node.type === 'BreakStatement' ||
    node.type === 'ClassDeclaration' ||
    node.type === 'ContinueStatement' ||
    node.type === 'DebuggerStatement' ||
    node.type === 'DeclareClass' ||
    node.type === 'DeclareVariable' ||
    node.type === 'DeclareFunction' ||
    node.type === 'DeclareInterface' ||
    node.type === 'DeclareModule' ||
    node.type === 'DeclareOpaqueType' ||
    node.type === 'DeclareTypeAlias' ||
    node.type === 'DoWhileStatement' ||
    node.type === 'EmptyStatement' ||
    node.type === 'EnumDeclaration' ||
    node.type === 'ExpressionStatement' ||
    node.type === 'ForInStatement' ||
    node.type === 'ForOfStatement' ||
    node.type === 'ForStatement' ||
    node.type === 'FunctionDeclaration' ||
    node.type === 'IfStatement' ||
    node.type === 'InterfaceDeclaration' ||
    node.type === 'LabeledStatement' ||
    node.type === 'MatchStatement' ||
    node.type === 'OpaqueType' ||
    node.type === 'RecordDeclaration' ||
    node.type === 'ReturnStatement' ||
    node.type === 'SwitchStatement' ||
    node.type === 'ThrowStatement' ||
    node.type === 'TryStatement' ||
    node.type === 'TypeAlias' ||
    node.type === 'VariableDeclaration' ||
    node.type === 'WhileStatement' ||
    node.type === 'WithStatement'
  );
}
