/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @generated
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run `buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen -- --estree-predicates`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {vars: "local"}], no-redeclare: 'error' */
/* global $NonMaybeType, Partial, $ReadOnly, $ReadOnlyArray, $FlowFixMe */

'use strict';

/*::
import type {
  ESNode,
  Token,
  AFunction,
  ClassMember,
  BigIntLiteral,
  BooleanLiteral,
  NullLiteral,
  NumericLiteral,
  RegExpLiteral,
  StringLiteral,
  Identifier,
  JSXIdentifier,
  JSXText,
  AnyTypeAnnotation,
  ArrayExpression,
  ArrayPattern,
  ArrayTypeAnnotation,
  ArrowFunctionExpression,
  AsConstExpression,
  AsExpression,
  AssignmentExpression,
  AssignmentPattern,
  AwaitExpression,
  BigIntLiteralTypeAnnotation,
  BigIntTypeAnnotation,
  BinaryExpression,
  BlockStatement,
  BooleanLiteralTypeAnnotation,
  BooleanTypeAnnotation,
  BreakStatement,
  CallExpression,
  CatchClause,
  ChainExpression,
  ClassBody,
  ClassDeclaration,
  ClassExpression,
  ClassImplements,
  ComponentDeclaration,
  ComponentParameter,
  ComponentTypeAnnotation,
  ComponentTypeParameter,
  ConditionalExpression,
  ConditionalTypeAnnotation,
  ContinueStatement,
  DebuggerStatement,
  DeclareClass,
  DeclareComponent,
  DeclaredPredicate,
  DeclareEnum,
  DeclareExportAllDeclaration,
  DeclareExportDeclaration,
  DeclareFunction,
  DeclareHook,
  DeclareInterface,
  DeclareModule,
  DeclareModuleExports,
  DeclareNamespace,
  DeclareOpaqueType,
  DeclareTypeAlias,
  DeclareVariable,
  Decorator,
  DoWhileStatement,
  EmptyStatement,
  EmptyTypeAnnotation,
  EnumBigIntBody,
  EnumBigIntMember,
  EnumBooleanBody,
  EnumBooleanMember,
  EnumDeclaration,
  EnumDefaultedMember,
  EnumNumberBody,
  EnumNumberMember,
  EnumStringBody,
  EnumStringMember,
  EnumSymbolBody,
  ExistsTypeAnnotation,
  ExportAllDeclaration,
  ExportDefaultDeclaration,
  ExportNamedDeclaration,
  ExportSpecifier,
  ExpressionStatement,
  ForInStatement,
  ForOfStatement,
  ForStatement,
  FunctionDeclaration,
  FunctionExpression,
  FunctionTypeAnnotation,
  FunctionTypeParam,
  GenericTypeAnnotation,
  HookDeclaration,
  HookTypeAnnotation,
  IfStatement,
  ImportAttribute,
  ImportDeclaration,
  ImportDefaultSpecifier,
  ImportExpression,
  ImportNamespaceSpecifier,
  ImportSpecifier,
  IndexedAccessType,
  InferredPredicate,
  InferTypeAnnotation,
  InterfaceDeclaration,
  InterfaceExtends,
  InterfaceTypeAnnotation,
  IntersectionTypeAnnotation,
  JSXAttribute,
  JSXClosingElement,
  JSXClosingFragment,
  JSXElement,
  JSXEmptyExpression,
  JSXExpressionContainer,
  JSXFragment,
  JSXMemberExpression,
  JSXNamespacedName,
  JSXOpeningElement,
  JSXOpeningFragment,
  JSXSpreadAttribute,
  JSXSpreadChild,
  KeyofTypeAnnotation,
  LabeledStatement,
  LogicalExpression,
  MatchArrayPattern,
  MatchAsPattern,
  MatchBindingPattern,
  MatchExpression,
  MatchExpressionCase,
  MatchIdentifierPattern,
  MatchInstanceObjectPattern,
  MatchInstancePattern,
  MatchLiteralPattern,
  MatchMemberPattern,
  MatchObjectPattern,
  MatchObjectPatternProperty,
  MatchOrPattern,
  MatchRestPattern,
  MatchStatement,
  MatchStatementCase,
  MatchUnaryPattern,
  MatchWildcardPattern,
  MemberExpression,
  MetaProperty,
  MethodDefinition,
  MixedTypeAnnotation,
  NeverTypeAnnotation,
  NewExpression,
  NullableTypeAnnotation,
  NullLiteralTypeAnnotation,
  NumberLiteralTypeAnnotation,
  NumberTypeAnnotation,
  ObjectExpression,
  ObjectPattern,
  ObjectTypeAnnotation,
  ObjectTypeCallProperty,
  ObjectTypeIndexer,
  ObjectTypeInternalSlot,
  ObjectTypeMappedTypeProperty,
  ObjectTypeProperty,
  ObjectTypeSpreadProperty,
  OpaqueType,
  OptionalIndexedAccessType,
  PrivateIdentifier,
  Program,
  Property,
  PropertyDefinition,
  QualifiedTypeIdentifier,
  QualifiedTypeofIdentifier,
  RecordDeclaration,
  RecordDeclarationBody,
  RecordDeclarationImplements,
  RecordDeclarationProperty,
  RecordDeclarationStaticProperty,
  RecordExpression,
  RecordExpressionProperties,
  RestElement,
  ReturnStatement,
  SequenceExpression,
  SpreadElement,
  StaticBlock,
  StringLiteralTypeAnnotation,
  StringTypeAnnotation,
  Super,
  SwitchCase,
  SwitchStatement,
  SymbolTypeAnnotation,
  TaggedTemplateExpression,
  TemplateElement,
  TemplateLiteral,
  ThisExpression,
  ThisTypeAnnotation,
  ThrowStatement,
  TryStatement,
  TupleTypeAnnotation,
  TupleTypeLabeledElement,
  TupleTypeSpreadElement,
  TypeAlias,
  TypeAnnotation,
  TypeCastExpression,
  TypeofTypeAnnotation,
  TypeOperator,
  TypeParameter,
  TypeParameterDeclaration,
  TypeParameterInstantiation,
  TypePredicate,
  UnaryExpression,
  UndefinedTypeAnnotation,
  UnionTypeAnnotation,
  UnknownTypeAnnotation,
  UpdateExpression,
  VariableDeclaration,
  VariableDeclarator,
  Variance,
  VoidTypeAnnotation,
  WhileStatement,
  WithStatement,
  YieldExpression,
  Literal,
  LineComment,
  BlockComment,
  MostTokens,
} from '../types';
*/


export function isIdentifier(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return node.type === 'Identifier';
}
  

export function isJSXIdentifier(node /*: ESNode | Token */) /*: implies node is (JSXIdentifier | MostTokens) */ {
  return node.type === 'JSXIdentifier';
}
  

export function isJSXText(node /*: ESNode | Token */) /*: implies node is (JSXText | MostTokens) */ {
  return node.type === 'JSXText';
}
  

export function isAnyTypeAnnotation(node /*: ESNode | Token */) /*: implies node is AnyTypeAnnotation */ {
  return node.type === 'AnyTypeAnnotation';
}
    

export function isArrayExpression(node /*: ESNode | Token */) /*: implies node is ArrayExpression */ {
  return node.type === 'ArrayExpression';
}
    

export function isArrayPattern(node /*: ESNode | Token */) /*: implies node is ArrayPattern */ {
  return node.type === 'ArrayPattern';
}
    

export function isArrayTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ArrayTypeAnnotation */ {
  return node.type === 'ArrayTypeAnnotation';
}
    

export function isArrowFunctionExpression(node /*: ESNode | Token */) /*: implies node is ArrowFunctionExpression */ {
  return node.type === 'ArrowFunctionExpression';
}
    

export function isAsConstExpression(node /*: ESNode | Token */) /*: implies node is AsConstExpression */ {
  return node.type === 'AsConstExpression';
}
    

export function isAsExpression(node /*: ESNode | Token */) /*: implies node is AsExpression */ {
  return node.type === 'AsExpression';
}
    

export function isAssignmentExpression(node /*: ESNode | Token */) /*: implies node is AssignmentExpression */ {
  return node.type === 'AssignmentExpression';
}
    

export function isAssignmentPattern(node /*: ESNode | Token */) /*: implies node is AssignmentPattern */ {
  return node.type === 'AssignmentPattern';
}
    

export function isAwaitExpression(node /*: ESNode | Token */) /*: implies node is AwaitExpression */ {
  return node.type === 'AwaitExpression';
}
    

export function isBigIntLiteralTypeAnnotation(node /*: ESNode | Token */) /*: implies node is BigIntLiteralTypeAnnotation */ {
  return node.type === 'BigIntLiteralTypeAnnotation';
}
    

export function isBigIntTypeAnnotation(node /*: ESNode | Token */) /*: implies node is BigIntTypeAnnotation */ {
  return node.type === 'BigIntTypeAnnotation';
}
    

export function isBinaryExpression(node /*: ESNode | Token */) /*: implies node is BinaryExpression */ {
  return node.type === 'BinaryExpression';
}
    

export function isBlockStatement(node /*: ESNode | Token */) /*: implies node is BlockStatement */ {
  return node.type === 'BlockStatement';
}
    

export function isBooleanLiteralTypeAnnotation(node /*: ESNode | Token */) /*: implies node is BooleanLiteralTypeAnnotation */ {
  return node.type === 'BooleanLiteralTypeAnnotation';
}
    

export function isBooleanTypeAnnotation(node /*: ESNode | Token */) /*: implies node is BooleanTypeAnnotation */ {
  return node.type === 'BooleanTypeAnnotation';
}
    

export function isBreakStatement(node /*: ESNode | Token */) /*: implies node is BreakStatement */ {
  return node.type === 'BreakStatement';
}
    

export function isCallExpression(node /*: ESNode | Token */) /*: implies node is CallExpression */ {
  return node.type === 'CallExpression';
}
    

export function isCatchClause(node /*: ESNode | Token */) /*: implies node is CatchClause */ {
  return node.type === 'CatchClause';
}
    

export function isChainExpression(node /*: ESNode | Token */) /*: implies node is ChainExpression */ {
  return node.type === 'ChainExpression';
}
    

export function isClassBody(node /*: ESNode | Token */) /*: implies node is ClassBody */ {
  return node.type === 'ClassBody';
}
    

export function isClassDeclaration(node /*: ESNode | Token */) /*: implies node is ClassDeclaration */ {
  return node.type === 'ClassDeclaration';
}
    

export function isClassExpression(node /*: ESNode | Token */) /*: implies node is ClassExpression */ {
  return node.type === 'ClassExpression';
}
    

export function isClassImplements(node /*: ESNode | Token */) /*: implies node is ClassImplements */ {
  return node.type === 'ClassImplements';
}
    

export function isComponentDeclaration(node /*: ESNode | Token */) /*: implies node is ComponentDeclaration */ {
  return node.type === 'ComponentDeclaration';
}
    

export function isComponentParameter(node /*: ESNode | Token */) /*: implies node is ComponentParameter */ {
  return node.type === 'ComponentParameter';
}
    

export function isComponentTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ComponentTypeAnnotation */ {
  return node.type === 'ComponentTypeAnnotation';
}
    

export function isComponentTypeParameter(node /*: ESNode | Token */) /*: implies node is ComponentTypeParameter */ {
  return node.type === 'ComponentTypeParameter';
}
    

export function isConditionalExpression(node /*: ESNode | Token */) /*: implies node is ConditionalExpression */ {
  return node.type === 'ConditionalExpression';
}
    

export function isConditionalTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ConditionalTypeAnnotation */ {
  return node.type === 'ConditionalTypeAnnotation';
}
    

export function isContinueStatement(node /*: ESNode | Token */) /*: implies node is ContinueStatement */ {
  return node.type === 'ContinueStatement';
}
    

export function isDebuggerStatement(node /*: ESNode | Token */) /*: implies node is DebuggerStatement */ {
  return node.type === 'DebuggerStatement';
}
    

export function isDeclareClass(node /*: ESNode | Token */) /*: implies node is DeclareClass */ {
  return node.type === 'DeclareClass';
}
    

export function isDeclareComponent(node /*: ESNode | Token */) /*: implies node is DeclareComponent */ {
  return node.type === 'DeclareComponent';
}
    

export function isDeclaredPredicate(node /*: ESNode | Token */) /*: implies node is DeclaredPredicate */ {
  return node.type === 'DeclaredPredicate';
}
    

export function isDeclareEnum(node /*: ESNode | Token */) /*: implies node is DeclareEnum */ {
  return node.type === 'DeclareEnum';
}
    

export function isDeclareExportAllDeclaration(node /*: ESNode | Token */) /*: implies node is DeclareExportAllDeclaration */ {
  return node.type === 'DeclareExportAllDeclaration';
}
    

export function isDeclareExportDeclaration(node /*: ESNode | Token */) /*: implies node is DeclareExportDeclaration */ {
  return node.type === 'DeclareExportDeclaration';
}
    

export function isDeclareFunction(node /*: ESNode | Token */) /*: implies node is DeclareFunction */ {
  return node.type === 'DeclareFunction';
}
    

export function isDeclareHook(node /*: ESNode | Token */) /*: implies node is DeclareHook */ {
  return node.type === 'DeclareHook';
}
    

export function isDeclareInterface(node /*: ESNode | Token */) /*: implies node is DeclareInterface */ {
  return node.type === 'DeclareInterface';
}
    

export function isDeclareModule(node /*: ESNode | Token */) /*: implies node is DeclareModule */ {
  return node.type === 'DeclareModule';
}
    

export function isDeclareModuleExports(node /*: ESNode | Token */) /*: implies node is DeclareModuleExports */ {
  return node.type === 'DeclareModuleExports';
}
    

export function isDeclareNamespace(node /*: ESNode | Token */) /*: implies node is DeclareNamespace */ {
  return node.type === 'DeclareNamespace';
}
    

export function isDeclareOpaqueType(node /*: ESNode | Token */) /*: implies node is DeclareOpaqueType */ {
  return node.type === 'DeclareOpaqueType';
}
    

export function isDeclareTypeAlias(node /*: ESNode | Token */) /*: implies node is DeclareTypeAlias */ {
  return node.type === 'DeclareTypeAlias';
}
    

export function isDeclareVariable(node /*: ESNode | Token */) /*: implies node is DeclareVariable */ {
  return node.type === 'DeclareVariable';
}
    

export function isDecorator(node /*: ESNode | Token */) /*: implies node is Decorator */ {
  return node.type === 'Decorator';
}
    

export function isDoWhileStatement(node /*: ESNode | Token */) /*: implies node is DoWhileStatement */ {
  return node.type === 'DoWhileStatement';
}
    

export function isEmptyStatement(node /*: ESNode | Token */) /*: implies node is EmptyStatement */ {
  return node.type === 'EmptyStatement';
}
    

export function isEmptyTypeAnnotation(node /*: ESNode | Token */) /*: implies node is EmptyTypeAnnotation */ {
  return node.type === 'EmptyTypeAnnotation';
}
    

export function isEnumBigIntBody(node /*: ESNode | Token */) /*: implies node is EnumBigIntBody */ {
  return node.type === 'EnumBigIntBody';
}
    

export function isEnumBigIntMember(node /*: ESNode | Token */) /*: implies node is EnumBigIntMember */ {
  return node.type === 'EnumBigIntMember';
}
    

export function isEnumBooleanBody(node /*: ESNode | Token */) /*: implies node is EnumBooleanBody */ {
  return node.type === 'EnumBooleanBody';
}
    

export function isEnumBooleanMember(node /*: ESNode | Token */) /*: implies node is EnumBooleanMember */ {
  return node.type === 'EnumBooleanMember';
}
    

export function isEnumDeclaration(node /*: ESNode | Token */) /*: implies node is EnumDeclaration */ {
  return node.type === 'EnumDeclaration';
}
    

export function isEnumDefaultedMember(node /*: ESNode | Token */) /*: implies node is EnumDefaultedMember */ {
  return node.type === 'EnumDefaultedMember';
}
    

export function isEnumNumberBody(node /*: ESNode | Token */) /*: implies node is EnumNumberBody */ {
  return node.type === 'EnumNumberBody';
}
    

export function isEnumNumberMember(node /*: ESNode | Token */) /*: implies node is EnumNumberMember */ {
  return node.type === 'EnumNumberMember';
}
    

export function isEnumStringBody(node /*: ESNode | Token */) /*: implies node is EnumStringBody */ {
  return node.type === 'EnumStringBody';
}
    

export function isEnumStringMember(node /*: ESNode | Token */) /*: implies node is EnumStringMember */ {
  return node.type === 'EnumStringMember';
}
    

export function isEnumSymbolBody(node /*: ESNode | Token */) /*: implies node is EnumSymbolBody */ {
  return node.type === 'EnumSymbolBody';
}
    

export function isExistsTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ExistsTypeAnnotation */ {
  return node.type === 'ExistsTypeAnnotation';
}
    

export function isExportAllDeclaration(node /*: ESNode | Token */) /*: implies node is ExportAllDeclaration */ {
  return node.type === 'ExportAllDeclaration';
}
    

export function isExportDefaultDeclaration(node /*: ESNode | Token */) /*: implies node is ExportDefaultDeclaration */ {
  return node.type === 'ExportDefaultDeclaration';
}
    

export function isExportNamedDeclaration(node /*: ESNode | Token */) /*: implies node is ExportNamedDeclaration */ {
  return node.type === 'ExportNamedDeclaration';
}
    

export function isExportSpecifier(node /*: ESNode | Token */) /*: implies node is ExportSpecifier */ {
  return node.type === 'ExportSpecifier';
}
    

export function isExpressionStatement(node /*: ESNode | Token */) /*: implies node is ExpressionStatement */ {
  return node.type === 'ExpressionStatement';
}
    

export function isForInStatement(node /*: ESNode | Token */) /*: implies node is ForInStatement */ {
  return node.type === 'ForInStatement';
}
    

export function isForOfStatement(node /*: ESNode | Token */) /*: implies node is ForOfStatement */ {
  return node.type === 'ForOfStatement';
}
    

export function isForStatement(node /*: ESNode | Token */) /*: implies node is ForStatement */ {
  return node.type === 'ForStatement';
}
    

export function isFunctionDeclaration(node /*: ESNode | Token */) /*: implies node is FunctionDeclaration */ {
  return node.type === 'FunctionDeclaration';
}
    

export function isFunctionExpression(node /*: ESNode | Token */) /*: implies node is FunctionExpression */ {
  return node.type === 'FunctionExpression';
}
    

export function isFunctionTypeAnnotation(node /*: ESNode | Token */) /*: implies node is FunctionTypeAnnotation */ {
  return node.type === 'FunctionTypeAnnotation';
}
    

export function isFunctionTypeParam(node /*: ESNode | Token */) /*: implies node is FunctionTypeParam */ {
  return node.type === 'FunctionTypeParam';
}
    

export function isGenericTypeAnnotation(node /*: ESNode | Token */) /*: implies node is GenericTypeAnnotation */ {
  return node.type === 'GenericTypeAnnotation';
}
    

export function isHookDeclaration(node /*: ESNode | Token */) /*: implies node is HookDeclaration */ {
  return node.type === 'HookDeclaration';
}
    

export function isHookTypeAnnotation(node /*: ESNode | Token */) /*: implies node is HookTypeAnnotation */ {
  return node.type === 'HookTypeAnnotation';
}
    

export function isIfStatement(node /*: ESNode | Token */) /*: implies node is IfStatement */ {
  return node.type === 'IfStatement';
}
    

export function isImportAttribute(node /*: ESNode | Token */) /*: implies node is ImportAttribute */ {
  return node.type === 'ImportAttribute';
}
    

export function isImportDeclaration(node /*: ESNode | Token */) /*: implies node is ImportDeclaration */ {
  return node.type === 'ImportDeclaration';
}
    

export function isImportDefaultSpecifier(node /*: ESNode | Token */) /*: implies node is ImportDefaultSpecifier */ {
  return node.type === 'ImportDefaultSpecifier';
}
    

export function isImportExpression(node /*: ESNode | Token */) /*: implies node is ImportExpression */ {
  return node.type === 'ImportExpression';
}
    

export function isImportNamespaceSpecifier(node /*: ESNode | Token */) /*: implies node is ImportNamespaceSpecifier */ {
  return node.type === 'ImportNamespaceSpecifier';
}
    

export function isImportSpecifier(node /*: ESNode | Token */) /*: implies node is ImportSpecifier */ {
  return node.type === 'ImportSpecifier';
}
    

export function isIndexedAccessType(node /*: ESNode | Token */) /*: implies node is IndexedAccessType */ {
  return node.type === 'IndexedAccessType';
}
    

export function isInferredPredicate(node /*: ESNode | Token */) /*: implies node is InferredPredicate */ {
  return node.type === 'InferredPredicate';
}
    

export function isInferTypeAnnotation(node /*: ESNode | Token */) /*: implies node is InferTypeAnnotation */ {
  return node.type === 'InferTypeAnnotation';
}
    

export function isInterfaceDeclaration(node /*: ESNode | Token */) /*: implies node is InterfaceDeclaration */ {
  return node.type === 'InterfaceDeclaration';
}
    

export function isInterfaceExtends(node /*: ESNode | Token */) /*: implies node is InterfaceExtends */ {
  return node.type === 'InterfaceExtends';
}
    

export function isInterfaceTypeAnnotation(node /*: ESNode | Token */) /*: implies node is InterfaceTypeAnnotation */ {
  return node.type === 'InterfaceTypeAnnotation';
}
    

export function isIntersectionTypeAnnotation(node /*: ESNode | Token */) /*: implies node is IntersectionTypeAnnotation */ {
  return node.type === 'IntersectionTypeAnnotation';
}
    

export function isJSXAttribute(node /*: ESNode | Token */) /*: implies node is JSXAttribute */ {
  return node.type === 'JSXAttribute';
}
    

export function isJSXClosingElement(node /*: ESNode | Token */) /*: implies node is JSXClosingElement */ {
  return node.type === 'JSXClosingElement';
}
    

export function isJSXClosingFragment(node /*: ESNode | Token */) /*: implies node is JSXClosingFragment */ {
  return node.type === 'JSXClosingFragment';
}
    

export function isJSXElement(node /*: ESNode | Token */) /*: implies node is JSXElement */ {
  return node.type === 'JSXElement';
}
    

export function isJSXEmptyExpression(node /*: ESNode | Token */) /*: implies node is JSXEmptyExpression */ {
  return node.type === 'JSXEmptyExpression';
}
    

export function isJSXExpressionContainer(node /*: ESNode | Token */) /*: implies node is JSXExpressionContainer */ {
  return node.type === 'JSXExpressionContainer';
}
    

export function isJSXFragment(node /*: ESNode | Token */) /*: implies node is JSXFragment */ {
  return node.type === 'JSXFragment';
}
    

export function isJSXMemberExpression(node /*: ESNode | Token */) /*: implies node is JSXMemberExpression */ {
  return node.type === 'JSXMemberExpression';
}
    

export function isJSXNamespacedName(node /*: ESNode | Token */) /*: implies node is JSXNamespacedName */ {
  return node.type === 'JSXNamespacedName';
}
    

export function isJSXOpeningElement(node /*: ESNode | Token */) /*: implies node is JSXOpeningElement */ {
  return node.type === 'JSXOpeningElement';
}
    

export function isJSXOpeningFragment(node /*: ESNode | Token */) /*: implies node is JSXOpeningFragment */ {
  return node.type === 'JSXOpeningFragment';
}
    

export function isJSXSpreadAttribute(node /*: ESNode | Token */) /*: implies node is JSXSpreadAttribute */ {
  return node.type === 'JSXSpreadAttribute';
}
    

export function isJSXSpreadChild(node /*: ESNode | Token */) /*: implies node is JSXSpreadChild */ {
  return node.type === 'JSXSpreadChild';
}
    

export function isKeyofTypeAnnotation(node /*: ESNode | Token */) /*: implies node is KeyofTypeAnnotation */ {
  return node.type === 'KeyofTypeAnnotation';
}
    

export function isLabeledStatement(node /*: ESNode | Token */) /*: implies node is LabeledStatement */ {
  return node.type === 'LabeledStatement';
}
    

export function isLogicalExpression(node /*: ESNode | Token */) /*: implies node is LogicalExpression */ {
  return node.type === 'LogicalExpression';
}
    

export function isMatchArrayPattern(node /*: ESNode | Token */) /*: implies node is MatchArrayPattern */ {
  return node.type === 'MatchArrayPattern';
}
    

export function isMatchAsPattern(node /*: ESNode | Token */) /*: implies node is MatchAsPattern */ {
  return node.type === 'MatchAsPattern';
}
    

export function isMatchBindingPattern(node /*: ESNode | Token */) /*: implies node is MatchBindingPattern */ {
  return node.type === 'MatchBindingPattern';
}
    

export function isMatchExpression(node /*: ESNode | Token */) /*: implies node is MatchExpression */ {
  return node.type === 'MatchExpression';
}
    

export function isMatchExpressionCase(node /*: ESNode | Token */) /*: implies node is MatchExpressionCase */ {
  return node.type === 'MatchExpressionCase';
}
    

export function isMatchIdentifierPattern(node /*: ESNode | Token */) /*: implies node is MatchIdentifierPattern */ {
  return node.type === 'MatchIdentifierPattern';
}
    

export function isMatchInstanceObjectPattern(node /*: ESNode | Token */) /*: implies node is MatchInstanceObjectPattern */ {
  return node.type === 'MatchInstanceObjectPattern';
}
    

export function isMatchInstancePattern(node /*: ESNode | Token */) /*: implies node is MatchInstancePattern */ {
  return node.type === 'MatchInstancePattern';
}
    

export function isMatchLiteralPattern(node /*: ESNode | Token */) /*: implies node is MatchLiteralPattern */ {
  return node.type === 'MatchLiteralPattern';
}
    

export function isMatchMemberPattern(node /*: ESNode | Token */) /*: implies node is MatchMemberPattern */ {
  return node.type === 'MatchMemberPattern';
}
    

export function isMatchObjectPattern(node /*: ESNode | Token */) /*: implies node is MatchObjectPattern */ {
  return node.type === 'MatchObjectPattern';
}
    

export function isMatchObjectPatternProperty(node /*: ESNode | Token */) /*: implies node is MatchObjectPatternProperty */ {
  return node.type === 'MatchObjectPatternProperty';
}
    

export function isMatchOrPattern(node /*: ESNode | Token */) /*: implies node is MatchOrPattern */ {
  return node.type === 'MatchOrPattern';
}
    

export function isMatchRestPattern(node /*: ESNode | Token */) /*: implies node is MatchRestPattern */ {
  return node.type === 'MatchRestPattern';
}
    

export function isMatchStatement(node /*: ESNode | Token */) /*: implies node is MatchStatement */ {
  return node.type === 'MatchStatement';
}
    

export function isMatchStatementCase(node /*: ESNode | Token */) /*: implies node is MatchStatementCase */ {
  return node.type === 'MatchStatementCase';
}
    

export function isMatchUnaryPattern(node /*: ESNode | Token */) /*: implies node is MatchUnaryPattern */ {
  return node.type === 'MatchUnaryPattern';
}
    

export function isMatchWildcardPattern(node /*: ESNode | Token */) /*: implies node is MatchWildcardPattern */ {
  return node.type === 'MatchWildcardPattern';
}
    

export function isMemberExpression(node /*: ESNode | Token */) /*: implies node is MemberExpression */ {
  return node.type === 'MemberExpression';
}
    

export function isMetaProperty(node /*: ESNode | Token */) /*: implies node is MetaProperty */ {
  return node.type === 'MetaProperty';
}
    

export function isMethodDefinition(node /*: ESNode | Token */) /*: implies node is MethodDefinition */ {
  return node.type === 'MethodDefinition';
}
    

export function isMixedTypeAnnotation(node /*: ESNode | Token */) /*: implies node is MixedTypeAnnotation */ {
  return node.type === 'MixedTypeAnnotation';
}
    

export function isNeverTypeAnnotation(node /*: ESNode | Token */) /*: implies node is NeverTypeAnnotation */ {
  return node.type === 'NeverTypeAnnotation';
}
    

export function isNewExpression(node /*: ESNode | Token */) /*: implies node is NewExpression */ {
  return node.type === 'NewExpression';
}
    

export function isNullableTypeAnnotation(node /*: ESNode | Token */) /*: implies node is NullableTypeAnnotation */ {
  return node.type === 'NullableTypeAnnotation';
}
    

export function isNullLiteralTypeAnnotation(node /*: ESNode | Token */) /*: implies node is NullLiteralTypeAnnotation */ {
  return node.type === 'NullLiteralTypeAnnotation';
}
    

export function isNumberLiteralTypeAnnotation(node /*: ESNode | Token */) /*: implies node is NumberLiteralTypeAnnotation */ {
  return node.type === 'NumberLiteralTypeAnnotation';
}
    

export function isNumberTypeAnnotation(node /*: ESNode | Token */) /*: implies node is NumberTypeAnnotation */ {
  return node.type === 'NumberTypeAnnotation';
}
    

export function isObjectExpression(node /*: ESNode | Token */) /*: implies node is ObjectExpression */ {
  return node.type === 'ObjectExpression';
}
    

export function isObjectPattern(node /*: ESNode | Token */) /*: implies node is ObjectPattern */ {
  return node.type === 'ObjectPattern';
}
    

export function isObjectTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ObjectTypeAnnotation */ {
  return node.type === 'ObjectTypeAnnotation';
}
    

export function isObjectTypeCallProperty(node /*: ESNode | Token */) /*: implies node is ObjectTypeCallProperty */ {
  return node.type === 'ObjectTypeCallProperty';
}
    

export function isObjectTypeIndexer(node /*: ESNode | Token */) /*: implies node is ObjectTypeIndexer */ {
  return node.type === 'ObjectTypeIndexer';
}
    

export function isObjectTypeInternalSlot(node /*: ESNode | Token */) /*: implies node is ObjectTypeInternalSlot */ {
  return node.type === 'ObjectTypeInternalSlot';
}
    

export function isObjectTypeMappedTypeProperty(node /*: ESNode | Token */) /*: implies node is ObjectTypeMappedTypeProperty */ {
  return node.type === 'ObjectTypeMappedTypeProperty';
}
    

export function isObjectTypeProperty(node /*: ESNode | Token */) /*: implies node is ObjectTypeProperty */ {
  return node.type === 'ObjectTypeProperty';
}
    

export function isObjectTypeSpreadProperty(node /*: ESNode | Token */) /*: implies node is ObjectTypeSpreadProperty */ {
  return node.type === 'ObjectTypeSpreadProperty';
}
    

export function isOpaqueType(node /*: ESNode | Token */) /*: implies node is OpaqueType */ {
  return node.type === 'OpaqueType';
}
    

export function isOptionalIndexedAccessType(node /*: ESNode | Token */) /*: implies node is OptionalIndexedAccessType */ {
  return node.type === 'OptionalIndexedAccessType';
}
    

export function isPrivateIdentifier(node /*: ESNode | Token */) /*: implies node is PrivateIdentifier */ {
  return node.type === 'PrivateIdentifier';
}
    

export function isProgram(node /*: ESNode | Token */) /*: implies node is Program */ {
  return node.type === 'Program';
}
    

export function isProperty(node /*: ESNode | Token */) /*: implies node is Property */ {
  return node.type === 'Property';
}
    

export function isPropertyDefinition(node /*: ESNode | Token */) /*: implies node is PropertyDefinition */ {
  return node.type === 'PropertyDefinition';
}
    

export function isQualifiedTypeIdentifier(node /*: ESNode | Token */) /*: implies node is QualifiedTypeIdentifier */ {
  return node.type === 'QualifiedTypeIdentifier';
}
    

export function isQualifiedTypeofIdentifier(node /*: ESNode | Token */) /*: implies node is QualifiedTypeofIdentifier */ {
  return node.type === 'QualifiedTypeofIdentifier';
}
    

export function isRecordDeclaration(node /*: ESNode | Token */) /*: implies node is RecordDeclaration */ {
  return node.type === 'RecordDeclaration';
}
    

export function isRecordDeclarationBody(node /*: ESNode | Token */) /*: implies node is RecordDeclarationBody */ {
  return node.type === 'RecordDeclarationBody';
}
    

export function isRecordDeclarationImplements(node /*: ESNode | Token */) /*: implies node is RecordDeclarationImplements */ {
  return node.type === 'RecordDeclarationImplements';
}
    

export function isRecordDeclarationProperty(node /*: ESNode | Token */) /*: implies node is RecordDeclarationProperty */ {
  return node.type === 'RecordDeclarationProperty';
}
    

export function isRecordDeclarationStaticProperty(node /*: ESNode | Token */) /*: implies node is RecordDeclarationStaticProperty */ {
  return node.type === 'RecordDeclarationStaticProperty';
}
    

export function isRecordExpression(node /*: ESNode | Token */) /*: implies node is RecordExpression */ {
  return node.type === 'RecordExpression';
}
    

export function isRecordExpressionProperties(node /*: ESNode | Token */) /*: implies node is RecordExpressionProperties */ {
  return node.type === 'RecordExpressionProperties';
}
    

export function isRestElement(node /*: ESNode | Token */) /*: implies node is RestElement */ {
  return node.type === 'RestElement';
}
    

export function isReturnStatement(node /*: ESNode | Token */) /*: implies node is ReturnStatement */ {
  return node.type === 'ReturnStatement';
}
    

export function isSequenceExpression(node /*: ESNode | Token */) /*: implies node is SequenceExpression */ {
  return node.type === 'SequenceExpression';
}
    

export function isSpreadElement(node /*: ESNode | Token */) /*: implies node is SpreadElement */ {
  return node.type === 'SpreadElement';
}
    

export function isStaticBlock(node /*: ESNode | Token */) /*: implies node is StaticBlock */ {
  return node.type === 'StaticBlock';
}
    

export function isStringLiteralTypeAnnotation(node /*: ESNode | Token */) /*: implies node is StringLiteralTypeAnnotation */ {
  return node.type === 'StringLiteralTypeAnnotation';
}
    

export function isStringTypeAnnotation(node /*: ESNode | Token */) /*: implies node is StringTypeAnnotation */ {
  return node.type === 'StringTypeAnnotation';
}
    

export function isSuper(node /*: ESNode | Token */) /*: implies node is Super */ {
  return node.type === 'Super';
}
    

export function isSwitchCase(node /*: ESNode | Token */) /*: implies node is SwitchCase */ {
  return node.type === 'SwitchCase';
}
    

export function isSwitchStatement(node /*: ESNode | Token */) /*: implies node is SwitchStatement */ {
  return node.type === 'SwitchStatement';
}
    

export function isSymbolTypeAnnotation(node /*: ESNode | Token */) /*: implies node is SymbolTypeAnnotation */ {
  return node.type === 'SymbolTypeAnnotation';
}
    

export function isTaggedTemplateExpression(node /*: ESNode | Token */) /*: implies node is TaggedTemplateExpression */ {
  return node.type === 'TaggedTemplateExpression';
}
    

export function isTemplateElement(node /*: ESNode | Token */) /*: implies node is TemplateElement */ {
  return node.type === 'TemplateElement';
}
    

export function isTemplateLiteral(node /*: ESNode | Token */) /*: implies node is TemplateLiteral */ {
  return node.type === 'TemplateLiteral';
}
    

export function isThisExpression(node /*: ESNode | Token */) /*: implies node is ThisExpression */ {
  return node.type === 'ThisExpression';
}
    

export function isThisTypeAnnotation(node /*: ESNode | Token */) /*: implies node is ThisTypeAnnotation */ {
  return node.type === 'ThisTypeAnnotation';
}
    

export function isThrowStatement(node /*: ESNode | Token */) /*: implies node is ThrowStatement */ {
  return node.type === 'ThrowStatement';
}
    

export function isTryStatement(node /*: ESNode | Token */) /*: implies node is TryStatement */ {
  return node.type === 'TryStatement';
}
    

export function isTupleTypeAnnotation(node /*: ESNode | Token */) /*: implies node is TupleTypeAnnotation */ {
  return node.type === 'TupleTypeAnnotation';
}
    

export function isTupleTypeLabeledElement(node /*: ESNode | Token */) /*: implies node is TupleTypeLabeledElement */ {
  return node.type === 'TupleTypeLabeledElement';
}
    

export function isTupleTypeSpreadElement(node /*: ESNode | Token */) /*: implies node is TupleTypeSpreadElement */ {
  return node.type === 'TupleTypeSpreadElement';
}
    

export function isTypeAlias(node /*: ESNode | Token */) /*: implies node is TypeAlias */ {
  return node.type === 'TypeAlias';
}
    

export function isTypeAnnotation(node /*: ESNode | Token */) /*: implies node is TypeAnnotation */ {
  return node.type === 'TypeAnnotation';
}
    

export function isTypeCastExpression(node /*: ESNode | Token */) /*: implies node is TypeCastExpression */ {
  return node.type === 'TypeCastExpression';
}
    

export function isTypeofTypeAnnotation(node /*: ESNode | Token */) /*: implies node is TypeofTypeAnnotation */ {
  return node.type === 'TypeofTypeAnnotation';
}
    

export function isTypeOperator(node /*: ESNode | Token */) /*: implies node is TypeOperator */ {
  return node.type === 'TypeOperator';
}
    

export function isTypeParameter(node /*: ESNode | Token */) /*: implies node is TypeParameter */ {
  return node.type === 'TypeParameter';
}
    

export function isTypeParameterDeclaration(node /*: ESNode | Token */) /*: implies node is TypeParameterDeclaration */ {
  return node.type === 'TypeParameterDeclaration';
}
    

export function isTypeParameterInstantiation(node /*: ESNode | Token */) /*: implies node is TypeParameterInstantiation */ {
  return node.type === 'TypeParameterInstantiation';
}
    

export function isTypePredicate(node /*: ESNode | Token */) /*: implies node is TypePredicate */ {
  return node.type === 'TypePredicate';
}
    

export function isUnaryExpression(node /*: ESNode | Token */) /*: implies node is UnaryExpression */ {
  return node.type === 'UnaryExpression';
}
    

export function isUndefinedTypeAnnotation(node /*: ESNode | Token */) /*: implies node is UndefinedTypeAnnotation */ {
  return node.type === 'UndefinedTypeAnnotation';
}
    

export function isUnionTypeAnnotation(node /*: ESNode | Token */) /*: implies node is UnionTypeAnnotation */ {
  return node.type === 'UnionTypeAnnotation';
}
    

export function isUnknownTypeAnnotation(node /*: ESNode | Token */) /*: implies node is UnknownTypeAnnotation */ {
  return node.type === 'UnknownTypeAnnotation';
}
    

export function isUpdateExpression(node /*: ESNode | Token */) /*: implies node is UpdateExpression */ {
  return node.type === 'UpdateExpression';
}
    

export function isVariableDeclaration(node /*: ESNode | Token */) /*: implies node is VariableDeclaration */ {
  return node.type === 'VariableDeclaration';
}
    

export function isVariableDeclarator(node /*: ESNode | Token */) /*: implies node is VariableDeclarator */ {
  return node.type === 'VariableDeclarator';
}
    

export function isVariance(node /*: ESNode | Token */) /*: implies node is Variance */ {
  return node.type === 'Variance';
}
    

export function isVoidTypeAnnotation(node /*: ESNode | Token */) /*: implies node is VoidTypeAnnotation */ {
  return node.type === 'VoidTypeAnnotation';
}
    

export function isWhileStatement(node /*: ESNode | Token */) /*: implies node is WhileStatement */ {
  return node.type === 'WhileStatement';
}
    

export function isWithStatement(node /*: ESNode | Token */) /*: implies node is WithStatement */ {
  return node.type === 'WithStatement';
}
    

export function isYieldExpression(node /*: ESNode | Token */) /*: implies node is YieldExpression */ {
  return node.type === 'YieldExpression';
}
    

export function isLiteral(node /*: ESNode | Token */) /*: implies node is Literal */ {
  return node.type === 'Literal';
}
    

export function isLineComment(node /*: ESNode | Token */) /*: implies node is (MostTokens | LineComment) */ {
  return node.type === 'Line';
}
    

export function isBlockComment(node /*: ESNode | Token */) /*: implies node is (MostTokens | BlockComment) */ {
  return node.type === 'Block';
}
    

export function isMinusToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '-';
}
      

export function isPlusToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '+';
}
      

export function isLogicalNotToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '!';
}
      

export function isUnaryNegationToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '~';
}
      

export function isTypeOfToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'typeof';
}
      

export function isVoidToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'void';
}
      

export function isDeleteToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'delete';
}
      

export function isLooseEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '==';
}
      

export function isLooseNotEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '!=';
}
      

export function isStrictEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '===';
}
      

export function isStrictNotEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '!==';
}
      

export function isLessThanToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '<';
}
      

export function isLessThanOrEqualToToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '<=';
}
      

export function isGreaterThanToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>';
}
      

export function isGreaterThanOrEqualToToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>=';
}
      

export function isBitwiseLeftShiftToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '<<';
}
      

export function isBitwiseRightShiftToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>>';
}
      

export function isBitwiseUnsignedRightShiftToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>>>';
}
      

export function isAsterixToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '*';
}
      

export function isForwardSlashToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '/';
}
      

export function isPercentToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '%';
}
      

export function isExponentiationToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '**';
}
      

export function isBitwiseORToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '|';
}
      

export function isBitwiseXORToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '^';
}
      

export function isBitwiseANDToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '&';
}
      

export function isInToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'in';
}
      

export function isInstanceOfToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'instanceof';
}
      

export function isLogicalORToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '||';
}
      

export function isLogicalANDToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '&&';
}
      

export function isNullishCoalesceToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '??';
}
      

export function isEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '=';
}
      

export function isPlusEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '+=';
}
      

export function isMinusEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '-=';
}
      

export function isMultiplyEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '*=';
}
      

export function isDivideEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '/=';
}
      

export function isRemainderEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '%=';
}
      

export function isExponentateEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '**=';
}
      

export function isBitwiseLeftShiftEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '<<=';
}
      

export function isBitwiseRightShiftEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>>=';
}
      

export function isBitwiseUnsignedRightShiftEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>>>=';
}
      

export function isBitwiseOREqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '|=';
}
      

export function isBitwiseXOREqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '^=';
}
      

export function isBitwiseANDEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '&=';
}
      

export function isLogicalOREqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '||=';
}
      

export function isLogicalANDEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '&&=';
}
      

export function isNullishCoalesceEqualToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '??=';
}
      

export function isIncrementToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '++';
}
      

export function isDecrementToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '--';
}
      

export function isUnionTypeToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '|';
}
      

export function isIntersectionTypeToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '&';
}
      

export function isBreakToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'break';
}
      

export function isCaseToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'case';
}
      

export function isCatchToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'catch';
}
      

export function isClassToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'class';
}
      

export function isConstToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'const';
}
      

export function isContinueToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'continue';
}
      

export function isDebuggerToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'debugger';
}
      

export function isDefaultToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'default';
}
      

export function isDoToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'do';
}
      

export function isElseToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'else';
}
      

export function isEnumToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'enum';
}
      

export function isExportToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'export';
}
      

export function isExtendsToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'extends';
}
      

export function isFinallyToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'finally';
}
      

export function isForToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'for';
}
      

export function isFunctionToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'function';
}
      

export function isIfToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'if';
}
      

export function isImplementsToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'implements';
}
      

export function isImportToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'import';
}
      

export function isInterfaceToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'interface';
}
      

export function isNewToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'new';
}
      

export function isReturnToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'return';
}
      

export function isStaticToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'static';
}
      

export function isSuperToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'super';
}
      

export function isSwitchToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'switch';
}
      

export function isThisToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'this';
}
      

export function isThrowToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'throw';
}
      

export function isTryToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'try';
}
      

export function isVarToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'var';
}
      

export function isWhileToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'while';
}
      

export function isWithToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'with';
}
      

export function isYieldToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Keyword' && node.value === 'yield';
}
      

export function isAsKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'as') ||
    (node.type === 'Keyword' && node.value === 'as')
  );
}
      

export function isAsyncKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'async') ||
    (node.type === 'Keyword' && node.value === 'async')
  );
}
      

export function isAwaitKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'await') ||
    (node.type === 'Keyword' && node.value === 'await')
  );
}
      

export function isDeclareKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'declare') ||
    (node.type === 'Keyword' && node.value === 'declare')
  );
}
      

export function isFromKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'from') ||
    (node.type === 'Keyword' && node.value === 'from')
  );
}
      

export function isGetKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'get') ||
    (node.type === 'Keyword' && node.value === 'get')
  );
}
      

export function isLetKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'let') ||
    (node.type === 'Keyword' && node.value === 'let')
  );
}
      

export function isModuleKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'module') ||
    (node.type === 'Keyword' && node.value === 'module')
  );
}
      

export function isOfKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'of') ||
    (node.type === 'Keyword' && node.value === 'of')
  );
}
      

export function isSetKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'set') ||
    (node.type === 'Keyword' && node.value === 'set')
  );
}
      

export function isTypeKeyword(node /*: ESNode | Token */) /*: implies node is (Identifier | MostTokens) */ {
  return (
    (node.type === 'Identifier' && node.name === 'type') ||
    (node.type === 'Keyword' && node.value === 'type')
  );
}
      

export function isCommaToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === ',';
}
      

export function isColonToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === ':';
}
      

export function isSemicolonToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === ';';
}
      

export function isDotToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '.';
}
      

export function isDotDotDotToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '...';
}
      

export function isOptionalChainToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '?.';
}
      

export function isQuestionMarkToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '?';
}
      

export function isOpeningParenthesisToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '(';
}
      

export function isClosingParenthesisToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === ')';
}
      

export function isOpeningCurlyBracketToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '{';
}
      

export function isClosingCurlyBracketToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '}';
}
      

export function isOpeningAngleBracketToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '<';
}
      

export function isClosingAngleBracketToken(node /*: ESNode | Token */) /*: implies node is MostTokens */ {
  return node.type === 'Punctuator' && node.value === '>';
}
      
