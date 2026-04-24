/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 * @generated
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run `yarn build`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {vars: "local"}], no-redeclare: 'error' */
/* global $NonMaybeType, Partial, $ReadOnly, $ReadOnlyArray, $FlowFixMe */

'use strict';

import type {
  ESNode,
  AnyTypeAnnotation as AnyTypeAnnotationType,
  ArrayExpression as ArrayExpressionType,
  ArrayPattern as ArrayPatternType,
  ArrayTypeAnnotation as ArrayTypeAnnotationType,
  AsConstExpression as AsConstExpressionType,
  AsExpression as AsExpressionType,
  AssignmentExpression as AssignmentExpressionType,
  AssignmentPattern as AssignmentPatternType,
  AwaitExpression as AwaitExpressionType,
  BigIntLiteralTypeAnnotation as BigIntLiteralTypeAnnotationType,
  BigIntTypeAnnotation as BigIntTypeAnnotationType,
  BinaryExpression as BinaryExpressionType,
  BooleanLiteralTypeAnnotation as BooleanLiteralTypeAnnotationType,
  BooleanTypeAnnotation as BooleanTypeAnnotationType,
  BreakStatement as BreakStatementType,
  CallExpression as CallExpressionType,
  CatchClause as CatchClauseType,
  ChainExpression as ChainExpressionType,
  ClassBody as ClassBodyType,
  ClassExpression as ClassExpressionType,
  ClassImplements as ClassImplementsType,
  ComponentDeclaration as ComponentDeclarationType,
  ComponentParameter as ComponentParameterType,
  ComponentTypeAnnotation as ComponentTypeAnnotationType,
  ComponentTypeParameter as ComponentTypeParameterType,
  ConditionalExpression as ConditionalExpressionType,
  ConditionalTypeAnnotation as ConditionalTypeAnnotationType,
  ContinueStatement as ContinueStatementType,
  DebuggerStatement as DebuggerStatementType,
  DeclareClass as DeclareClassType,
  DeclareComponent as DeclareComponentType,
  DeclaredPredicate as DeclaredPredicateType,
  DeclareEnum as DeclareEnumType,
  DeclareExportAllDeclaration as DeclareExportAllDeclarationType,
  DeclareInterface as DeclareInterfaceType,
  DeclareModule as DeclareModuleType,
  DeclareModuleExports as DeclareModuleExportsType,
  DeclareNamespace as DeclareNamespaceType,
  DeclareOpaqueType as DeclareOpaqueTypeType,
  DeclareTypeAlias as DeclareTypeAliasType,
  DeclareVariable as DeclareVariableType,
  Decorator as DecoratorType,
  DoWhileStatement as DoWhileStatementType,
  EmptyStatement as EmptyStatementType,
  EmptyTypeAnnotation as EmptyTypeAnnotationType,
  EnumBigIntBody as EnumBigIntBodyType,
  EnumBigIntMember as EnumBigIntMemberType,
  EnumBooleanBody as EnumBooleanBodyType,
  EnumBooleanMember as EnumBooleanMemberType,
  EnumDeclaration as EnumDeclarationType,
  EnumDefaultedMember as EnumDefaultedMemberType,
  EnumNumberBody as EnumNumberBodyType,
  EnumNumberMember as EnumNumberMemberType,
  EnumStringBody as EnumStringBodyType,
  EnumStringMember as EnumStringMemberType,
  EnumSymbolBody as EnumSymbolBodyType,
  ExistsTypeAnnotation as ExistsTypeAnnotationType,
  ExportAllDeclaration as ExportAllDeclarationType,
  ExportDefaultDeclaration as ExportDefaultDeclarationType,
  ExportSpecifier as ExportSpecifierType,
  ExpressionStatement as ExpressionStatementType,
  ForInStatement as ForInStatementType,
  ForOfStatement as ForOfStatementType,
  ForStatement as ForStatementType,
  FunctionDeclaration as FunctionDeclarationType,
  FunctionExpression as FunctionExpressionType,
  FunctionTypeAnnotation as FunctionTypeAnnotationType,
  FunctionTypeParam as FunctionTypeParamType,
  GenericTypeAnnotation as GenericTypeAnnotationType,
  HookDeclaration as HookDeclarationType,
  HookTypeAnnotation as HookTypeAnnotationType,
  IfStatement as IfStatementType,
  ImportAttribute as ImportAttributeType,
  ImportDeclaration as ImportDeclarationType,
  ImportDefaultSpecifier as ImportDefaultSpecifierType,
  ImportExpression as ImportExpressionType,
  ImportNamespaceSpecifier as ImportNamespaceSpecifierType,
  ImportSpecifier as ImportSpecifierType,
  IndexedAccessType as IndexedAccessTypeType,
  InferredPredicate as InferredPredicateType,
  InferTypeAnnotation as InferTypeAnnotationType,
  InterfaceDeclaration as InterfaceDeclarationType,
  InterfaceExtends as InterfaceExtendsType,
  InterfaceTypeAnnotation as InterfaceTypeAnnotationType,
  IntersectionTypeAnnotation as IntersectionTypeAnnotationType,
  JSXAttribute as JSXAttributeType,
  JSXClosingElement as JSXClosingElementType,
  JSXClosingFragment as JSXClosingFragmentType,
  JSXElement as JSXElementType,
  JSXEmptyExpression as JSXEmptyExpressionType,
  JSXExpressionContainer as JSXExpressionContainerType,
  JSXFragment as JSXFragmentType,
  JSXIdentifier as JSXIdentifierType,
  JSXMemberExpression as JSXMemberExpressionType,
  JSXNamespacedName as JSXNamespacedNameType,
  JSXOpeningElement as JSXOpeningElementType,
  JSXOpeningFragment as JSXOpeningFragmentType,
  JSXSpreadAttribute as JSXSpreadAttributeType,
  JSXSpreadChild as JSXSpreadChildType,
  JSXText as JSXTextType,
  KeyofTypeAnnotation as KeyofTypeAnnotationType,
  LabeledStatement as LabeledStatementType,
  LogicalExpression as LogicalExpressionType,
  MatchArrayPattern as MatchArrayPatternType,
  MatchAsPattern as MatchAsPatternType,
  MatchBindingPattern as MatchBindingPatternType,
  MatchExpression as MatchExpressionType,
  MatchExpressionCase as MatchExpressionCaseType,
  MatchIdentifierPattern as MatchIdentifierPatternType,
  MatchInstanceObjectPattern as MatchInstanceObjectPatternType,
  MatchInstancePattern as MatchInstancePatternType,
  MatchLiteralPattern as MatchLiteralPatternType,
  MatchMemberPattern as MatchMemberPatternType,
  MatchObjectPattern as MatchObjectPatternType,
  MatchObjectPatternProperty as MatchObjectPatternPropertyType,
  MatchOrPattern as MatchOrPatternType,
  MatchRestPattern as MatchRestPatternType,
  MatchStatement as MatchStatementType,
  MatchStatementCase as MatchStatementCaseType,
  MatchUnaryPattern as MatchUnaryPatternType,
  MatchWildcardPattern as MatchWildcardPatternType,
  MetaProperty as MetaPropertyType,
  MethodDefinition as MethodDefinitionType,
  MixedTypeAnnotation as MixedTypeAnnotationType,
  NeverTypeAnnotation as NeverTypeAnnotationType,
  NewExpression as NewExpressionType,
  NullableTypeAnnotation as NullableTypeAnnotationType,
  NullLiteralTypeAnnotation as NullLiteralTypeAnnotationType,
  NumberLiteralTypeAnnotation as NumberLiteralTypeAnnotationType,
  NumberTypeAnnotation as NumberTypeAnnotationType,
  ObjectExpression as ObjectExpressionType,
  ObjectPattern as ObjectPatternType,
  ObjectTypeAnnotation as ObjectTypeAnnotationType,
  ObjectTypeCallProperty as ObjectTypeCallPropertyType,
  ObjectTypeIndexer as ObjectTypeIndexerType,
  ObjectTypeInternalSlot as ObjectTypeInternalSlotType,
  ObjectTypeMappedTypeProperty as ObjectTypeMappedTypePropertyType,
  ObjectTypeSpreadProperty as ObjectTypeSpreadPropertyType,
  OpaqueType as OpaqueTypeType,
  OptionalIndexedAccessType as OptionalIndexedAccessTypeType,
  PrivateIdentifier as PrivateIdentifierType,
  Property as PropertyType,
  PropertyDefinition as PropertyDefinitionType,
  QualifiedTypeIdentifier as QualifiedTypeIdentifierType,
  QualifiedTypeofIdentifier as QualifiedTypeofIdentifierType,
  RecordDeclaration as RecordDeclarationType,
  RecordDeclarationBody as RecordDeclarationBodyType,
  RecordDeclarationImplements as RecordDeclarationImplementsType,
  RecordDeclarationProperty as RecordDeclarationPropertyType,
  RecordDeclarationStaticProperty as RecordDeclarationStaticPropertyType,
  RecordExpression as RecordExpressionType,
  RecordExpressionProperties as RecordExpressionPropertiesType,
  RestElement as RestElementType,
  ReturnStatement as ReturnStatementType,
  SequenceExpression as SequenceExpressionType,
  SpreadElement as SpreadElementType,
  StaticBlock as StaticBlockType,
  StringLiteralTypeAnnotation as StringLiteralTypeAnnotationType,
  StringTypeAnnotation as StringTypeAnnotationType,
  Super as SuperType,
  SwitchCase as SwitchCaseType,
  SwitchStatement as SwitchStatementType,
  SymbolTypeAnnotation as SymbolTypeAnnotationType,
  TaggedTemplateExpression as TaggedTemplateExpressionType,
  TemplateLiteral as TemplateLiteralType,
  ThisExpression as ThisExpressionType,
  ThisTypeAnnotation as ThisTypeAnnotationType,
  ThrowStatement as ThrowStatementType,
  TryStatement as TryStatementType,
  TupleTypeAnnotation as TupleTypeAnnotationType,
  TupleTypeLabeledElement as TupleTypeLabeledElementType,
  TupleTypeSpreadElement as TupleTypeSpreadElementType,
  TypeAlias as TypeAliasType,
  TypeAnnotation as TypeAnnotationType,
  TypeCastExpression as TypeCastExpressionType,
  TypeofTypeAnnotation as TypeofTypeAnnotationType,
  TypeOperator as TypeOperatorType,
  TypeParameter as TypeParameterType,
  TypeParameterDeclaration as TypeParameterDeclarationType,
  TypeParameterInstantiation as TypeParameterInstantiationType,
  TypePredicate as TypePredicateType,
  UnaryExpression as UnaryExpressionType,
  UndefinedTypeAnnotation as UndefinedTypeAnnotationType,
  UnionTypeAnnotation as UnionTypeAnnotationType,
  UnknownTypeAnnotation as UnknownTypeAnnotationType,
  UpdateExpression as UpdateExpressionType,
  VariableDeclaration as VariableDeclarationType,
  VariableDeclarator as VariableDeclaratorType,
  Variance as VarianceType,
  VoidTypeAnnotation as VoidTypeAnnotationType,
  WhileStatement as WhileStatementType,
  WithStatement as WithStatementType,
  YieldExpression as YieldExpressionType,
} from 'hermes-estree';
import type {DetachedNode, MaybeDetachedNode} from '../detachedNode';

import {
  asDetachedNodeForCodeGen,
  detachedProps,
  setParentPointersInDirectChildren,
} from '../detachedNode';

export type AnyTypeAnnotationProps = {};

export type ArrayExpressionProps = {
  +elements: $ReadOnlyArray<
    MaybeDetachedNode<ArrayExpressionType['elements'][number]>,
  >,
  +trailingComma: ArrayExpressionType['trailingComma'],
};

export type ArrayPatternProps = {
  +elements: $ReadOnlyArray<
    MaybeDetachedNode<ArrayPatternType['elements'][number]>,
  >,
  +typeAnnotation?: ?MaybeDetachedNode<ArrayPatternType['typeAnnotation']>,
};

export type ArrayTypeAnnotationProps = {
  +elementType: MaybeDetachedNode<ArrayTypeAnnotationType['elementType']>,
};

export type AsConstExpressionProps = {
  +expression: MaybeDetachedNode<AsConstExpressionType['expression']>,
};

export type AsExpressionProps = {
  +expression: MaybeDetachedNode<AsExpressionType['expression']>,
  +typeAnnotation: MaybeDetachedNode<AsExpressionType['typeAnnotation']>,
};

export type AssignmentExpressionProps = {
  +operator: AssignmentExpressionType['operator'],
  +left: MaybeDetachedNode<AssignmentExpressionType['left']>,
  +right: MaybeDetachedNode<AssignmentExpressionType['right']>,
};

export type AssignmentPatternProps = {
  +left: MaybeDetachedNode<AssignmentPatternType['left']>,
  +right: MaybeDetachedNode<AssignmentPatternType['right']>,
};

export type AwaitExpressionProps = {
  +argument: MaybeDetachedNode<AwaitExpressionType['argument']>,
};

export type BigIntLiteralTypeAnnotationProps = {
  +raw: BigIntLiteralTypeAnnotationType['raw'],
};

export type BigIntTypeAnnotationProps = {};

export type BinaryExpressionProps = {
  +left: MaybeDetachedNode<BinaryExpressionType['left']>,
  +right: MaybeDetachedNode<BinaryExpressionType['right']>,
  +operator: BinaryExpressionType['operator'],
};

export type BooleanLiteralTypeAnnotationProps = {
  +value: BooleanLiteralTypeAnnotationType['value'],
  +raw: BooleanLiteralTypeAnnotationType['raw'],
};

export type BooleanTypeAnnotationProps = {};

export type BreakStatementProps = {
  +label?: ?MaybeDetachedNode<BreakStatementType['label']>,
};

export type CallExpressionProps = {
  +callee: MaybeDetachedNode<CallExpressionType['callee']>,
  +typeArguments?: ?MaybeDetachedNode<CallExpressionType['typeArguments']>,
  +arguments: $ReadOnlyArray<
    MaybeDetachedNode<CallExpressionType['arguments'][number]>,
  >,
};

export type CatchClauseProps = {
  +param?: ?MaybeDetachedNode<CatchClauseType['param']>,
  +body: MaybeDetachedNode<CatchClauseType['body']>,
};

export type ChainExpressionProps = {
  +expression: MaybeDetachedNode<ChainExpressionType['expression']>,
};

export type ClassBodyProps = {
  +body: $ReadOnlyArray<MaybeDetachedNode<ClassBodyType['body'][number]>>,
};

export type ClassExpressionProps = {
  +id?: ?MaybeDetachedNode<ClassExpressionType['id']>,
  +typeParameters?: ?MaybeDetachedNode<ClassExpressionType['typeParameters']>,
  +superClass?: ?MaybeDetachedNode<ClassExpressionType['superClass']>,
  +superTypeArguments?: ?MaybeDetachedNode<
    ClassExpressionType['superTypeArguments'],
  >,
  +implements: $ReadOnlyArray<
    MaybeDetachedNode<ClassExpressionType['implements'][number]>,
  >,
  +decorators: $ReadOnlyArray<
    MaybeDetachedNode<ClassExpressionType['decorators'][number]>,
  >,
  +body: MaybeDetachedNode<ClassExpressionType['body']>,
};

export type ClassImplementsProps = {
  +id: MaybeDetachedNode<ClassImplementsType['id']>,
  +typeParameters?: ?MaybeDetachedNode<ClassImplementsType['typeParameters']>,
};

export type ComponentDeclarationProps = {
  +id: MaybeDetachedNode<ComponentDeclarationType['id']>,
  +params: $ReadOnlyArray<
    MaybeDetachedNode<ComponentDeclarationType['params'][number]>,
  >,
  +body: MaybeDetachedNode<ComponentDeclarationType['body']>,
  +typeParameters?: ?MaybeDetachedNode<
    ComponentDeclarationType['typeParameters'],
  >,
  +rendersType?: ?MaybeDetachedNode<ComponentDeclarationType['rendersType']>,
  +async: ComponentDeclarationType['async'],
};

export type ComponentParameterProps = {
  +name: MaybeDetachedNode<ComponentParameterType['name']>,
  +local: MaybeDetachedNode<ComponentParameterType['local']>,
  +shorthand: ComponentParameterType['shorthand'],
};

export type ComponentTypeAnnotationProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<ComponentTypeAnnotationType['params'][number]>,
  >,
  +rest?: ?MaybeDetachedNode<ComponentTypeAnnotationType['rest']>,
  +typeParameters?: ?MaybeDetachedNode<
    ComponentTypeAnnotationType['typeParameters'],
  >,
  +rendersType?: ?MaybeDetachedNode<ComponentTypeAnnotationType['rendersType']>,
};

export type ComponentTypeParameterProps = {
  +name?: ?MaybeDetachedNode<ComponentTypeParameterType['name']>,
  +typeAnnotation: MaybeDetachedNode<
    ComponentTypeParameterType['typeAnnotation'],
  >,
  +optional: ComponentTypeParameterType['optional'],
};

export type ConditionalExpressionProps = {
  +test: MaybeDetachedNode<ConditionalExpressionType['test']>,
  +alternate: MaybeDetachedNode<ConditionalExpressionType['alternate']>,
  +consequent: MaybeDetachedNode<ConditionalExpressionType['consequent']>,
};

export type ConditionalTypeAnnotationProps = {
  +checkType: MaybeDetachedNode<ConditionalTypeAnnotationType['checkType']>,
  +extendsType: MaybeDetachedNode<ConditionalTypeAnnotationType['extendsType']>,
  +trueType: MaybeDetachedNode<ConditionalTypeAnnotationType['trueType']>,
  +falseType: MaybeDetachedNode<ConditionalTypeAnnotationType['falseType']>,
};

export type ContinueStatementProps = {
  +label?: ?MaybeDetachedNode<ContinueStatementType['label']>,
};

export type DebuggerStatementProps = {};

export type DeclareClassProps = {
  +id: MaybeDetachedNode<DeclareClassType['id']>,
  +typeParameters?: ?MaybeDetachedNode<DeclareClassType['typeParameters']>,
  +extends: $ReadOnlyArray<
    MaybeDetachedNode<DeclareClassType['extends'][number]>,
  >,
  +implements: $ReadOnlyArray<
    MaybeDetachedNode<DeclareClassType['implements'][number]>,
  >,
  +mixins: $ReadOnlyArray<
    MaybeDetachedNode<DeclareClassType['mixins'][number]>,
  >,
  +body: MaybeDetachedNode<DeclareClassType['body']>,
};

export type DeclareComponentProps = {
  +id: MaybeDetachedNode<DeclareComponentType['id']>,
  +params: $ReadOnlyArray<
    MaybeDetachedNode<DeclareComponentType['params'][number]>,
  >,
  +rest?: ?MaybeDetachedNode<DeclareComponentType['rest']>,
  +typeParameters?: ?MaybeDetachedNode<DeclareComponentType['typeParameters']>,
  +rendersType?: ?MaybeDetachedNode<DeclareComponentType['rendersType']>,
};

export type DeclaredPredicateProps = {
  +value: MaybeDetachedNode<DeclaredPredicateType['value']>,
};

export type DeclareEnumProps = {
  +id: MaybeDetachedNode<DeclareEnumType['id']>,
  +body: MaybeDetachedNode<DeclareEnumType['body']>,
};

export type DeclareExportAllDeclarationProps = {
  +source: MaybeDetachedNode<DeclareExportAllDeclarationType['source']>,
};

export type DeclareInterfaceProps = {
  +id: MaybeDetachedNode<DeclareInterfaceType['id']>,
  +typeParameters?: ?MaybeDetachedNode<DeclareInterfaceType['typeParameters']>,
  +extends: $ReadOnlyArray<
    MaybeDetachedNode<DeclareInterfaceType['extends'][number]>,
  >,
  +body: MaybeDetachedNode<DeclareInterfaceType['body']>,
};

export type DeclareModuleProps = {
  +id: MaybeDetachedNode<DeclareModuleType['id']>,
  +body: MaybeDetachedNode<DeclareModuleType['body']>,
};

export type DeclareModuleExportsProps = {
  +typeAnnotation: MaybeDetachedNode<
    DeclareModuleExportsType['typeAnnotation'],
  >,
};

export type DeclareNamespaceProps = {
  +id: MaybeDetachedNode<DeclareNamespaceType['id']>,
  +body: MaybeDetachedNode<DeclareNamespaceType['body']>,
};

export type DeclareOpaqueTypeProps = {
  +id: MaybeDetachedNode<DeclareOpaqueTypeType['id']>,
  +typeParameters?: ?MaybeDetachedNode<DeclareOpaqueTypeType['typeParameters']>,
  +impltype?: ?MaybeDetachedNode<DeclareOpaqueTypeType['impltype']>,
  +lowerBound?: ?MaybeDetachedNode<DeclareOpaqueTypeType['lowerBound']>,
  +upperBound?: ?MaybeDetachedNode<DeclareOpaqueTypeType['upperBound']>,
  +supertype?: ?MaybeDetachedNode<DeclareOpaqueTypeType['supertype']>,
};

export type DeclareTypeAliasProps = {
  +id: MaybeDetachedNode<DeclareTypeAliasType['id']>,
  +typeParameters?: ?MaybeDetachedNode<DeclareTypeAliasType['typeParameters']>,
  +right: MaybeDetachedNode<DeclareTypeAliasType['right']>,
};

export type DeclareVariableProps = {
  +id: MaybeDetachedNode<DeclareVariableType['id']>,
  +kind: DeclareVariableType['kind'],
};

export type DecoratorProps = {
  +expression: MaybeDetachedNode<DecoratorType['expression']>,
};

export type DoWhileStatementProps = {
  +body: MaybeDetachedNode<DoWhileStatementType['body']>,
  +test: MaybeDetachedNode<DoWhileStatementType['test']>,
};

export type EmptyStatementProps = {};

export type EmptyTypeAnnotationProps = {};

export type EnumBigIntBodyProps = {
  +members: $ReadOnlyArray<
    MaybeDetachedNode<EnumBigIntBodyType['members'][number]>,
  >,
  +explicitType: EnumBigIntBodyType['explicitType'],
  +hasUnknownMembers: EnumBigIntBodyType['hasUnknownMembers'],
};

export type EnumBigIntMemberProps = {
  +id: MaybeDetachedNode<EnumBigIntMemberType['id']>,
  +init: MaybeDetachedNode<EnumBigIntMemberType['init']>,
};

export type EnumBooleanBodyProps = {
  +members: $ReadOnlyArray<
    MaybeDetachedNode<EnumBooleanBodyType['members'][number]>,
  >,
  +explicitType: EnumBooleanBodyType['explicitType'],
  +hasUnknownMembers: EnumBooleanBodyType['hasUnknownMembers'],
};

export type EnumBooleanMemberProps = {
  +id: MaybeDetachedNode<EnumBooleanMemberType['id']>,
  +init: MaybeDetachedNode<EnumBooleanMemberType['init']>,
};

export type EnumDeclarationProps = {
  +id: MaybeDetachedNode<EnumDeclarationType['id']>,
  +body: MaybeDetachedNode<EnumDeclarationType['body']>,
};

export type EnumDefaultedMemberProps = {
  +id: MaybeDetachedNode<EnumDefaultedMemberType['id']>,
};

export type EnumNumberBodyProps = {
  +members: $ReadOnlyArray<
    MaybeDetachedNode<EnumNumberBodyType['members'][number]>,
  >,
  +explicitType: EnumNumberBodyType['explicitType'],
  +hasUnknownMembers: EnumNumberBodyType['hasUnknownMembers'],
};

export type EnumNumberMemberProps = {
  +id: MaybeDetachedNode<EnumNumberMemberType['id']>,
  +init: MaybeDetachedNode<EnumNumberMemberType['init']>,
};

export type EnumStringBodyProps = {
  +members: $ReadOnlyArray<
    MaybeDetachedNode<EnumStringBodyType['members'][number]>,
  >,
  +explicitType: EnumStringBodyType['explicitType'],
  +hasUnknownMembers: EnumStringBodyType['hasUnknownMembers'],
};

export type EnumStringMemberProps = {
  +id: MaybeDetachedNode<EnumStringMemberType['id']>,
  +init: MaybeDetachedNode<EnumStringMemberType['init']>,
};

export type EnumSymbolBodyProps = {
  +members: $ReadOnlyArray<
    MaybeDetachedNode<EnumSymbolBodyType['members'][number]>,
  >,
  +hasUnknownMembers: EnumSymbolBodyType['hasUnknownMembers'],
};

export type ExistsTypeAnnotationProps = {};

export type ExportAllDeclarationProps = {
  +exported?: ?MaybeDetachedNode<ExportAllDeclarationType['exported']>,
  +source: MaybeDetachedNode<ExportAllDeclarationType['source']>,
  +exportKind: ExportAllDeclarationType['exportKind'],
};

export type ExportDefaultDeclarationProps = {
  +declaration: MaybeDetachedNode<ExportDefaultDeclarationType['declaration']>,
};

export type ExportSpecifierProps = {
  +exported: MaybeDetachedNode<ExportSpecifierType['exported']>,
  +local: MaybeDetachedNode<ExportSpecifierType['local']>,
};

export type ExpressionStatementProps = {
  +expression: MaybeDetachedNode<ExpressionStatementType['expression']>,
  +directive?: ?ExpressionStatementType['directive'],
};

export type ForInStatementProps = {
  +left: MaybeDetachedNode<ForInStatementType['left']>,
  +right: MaybeDetachedNode<ForInStatementType['right']>,
  +body: MaybeDetachedNode<ForInStatementType['body']>,
};

export type ForOfStatementProps = {
  +left: MaybeDetachedNode<ForOfStatementType['left']>,
  +right: MaybeDetachedNode<ForOfStatementType['right']>,
  +body: MaybeDetachedNode<ForOfStatementType['body']>,
  +await: ForOfStatementType['await'],
};

export type ForStatementProps = {
  +init?: ?MaybeDetachedNode<ForStatementType['init']>,
  +test?: ?MaybeDetachedNode<ForStatementType['test']>,
  +update?: ?MaybeDetachedNode<ForStatementType['update']>,
  +body: MaybeDetachedNode<ForStatementType['body']>,
};

export type FunctionDeclarationProps = {
  +id?: ?MaybeDetachedNode<FunctionDeclarationType['id']>,
  +params: $ReadOnlyArray<
    MaybeDetachedNode<FunctionDeclarationType['params'][number]>,
  >,
  +body: MaybeDetachedNode<FunctionDeclarationType['body']>,
  +typeParameters?: ?MaybeDetachedNode<
    FunctionDeclarationType['typeParameters'],
  >,
  +returnType?: ?MaybeDetachedNode<FunctionDeclarationType['returnType']>,
  +predicate?: ?MaybeDetachedNode<FunctionDeclarationType['predicate']>,
  +generator: FunctionDeclarationType['generator'],
  +async: FunctionDeclarationType['async'],
};

export type FunctionExpressionProps = {
  +id?: ?MaybeDetachedNode<FunctionExpressionType['id']>,
  +params: $ReadOnlyArray<
    MaybeDetachedNode<FunctionExpressionType['params'][number]>,
  >,
  +body: MaybeDetachedNode<FunctionExpressionType['body']>,
  +typeParameters?: ?MaybeDetachedNode<
    FunctionExpressionType['typeParameters'],
  >,
  +returnType?: ?MaybeDetachedNode<FunctionExpressionType['returnType']>,
  +predicate?: ?MaybeDetachedNode<FunctionExpressionType['predicate']>,
  +generator: FunctionExpressionType['generator'],
  +async: FunctionExpressionType['async'],
};

export type FunctionTypeAnnotationProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<FunctionTypeAnnotationType['params'][number]>,
  >,
  +this?: ?MaybeDetachedNode<FunctionTypeAnnotationType['this']>,
  +returnType: MaybeDetachedNode<FunctionTypeAnnotationType['returnType']>,
  +rest?: ?MaybeDetachedNode<FunctionTypeAnnotationType['rest']>,
  +typeParameters?: ?MaybeDetachedNode<
    FunctionTypeAnnotationType['typeParameters'],
  >,
};

export type FunctionTypeParamProps = {
  +name?: ?MaybeDetachedNode<FunctionTypeParamType['name']>,
  +typeAnnotation: MaybeDetachedNode<FunctionTypeParamType['typeAnnotation']>,
  +optional: FunctionTypeParamType['optional'],
};

export type GenericTypeAnnotationProps = {
  +id: MaybeDetachedNode<GenericTypeAnnotationType['id']>,
  +typeParameters?: ?MaybeDetachedNode<
    GenericTypeAnnotationType['typeParameters'],
  >,
};

export type HookDeclarationProps = {
  +id: MaybeDetachedNode<HookDeclarationType['id']>,
  +params: $ReadOnlyArray<
    MaybeDetachedNode<HookDeclarationType['params'][number]>,
  >,
  +body: MaybeDetachedNode<HookDeclarationType['body']>,
  +typeParameters?: ?MaybeDetachedNode<HookDeclarationType['typeParameters']>,
  +returnType?: ?MaybeDetachedNode<HookDeclarationType['returnType']>,
  +async: HookDeclarationType['async'],
};

export type HookTypeAnnotationProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<HookTypeAnnotationType['params'][number]>,
  >,
  +returnType: MaybeDetachedNode<HookTypeAnnotationType['returnType']>,
  +rest?: ?MaybeDetachedNode<HookTypeAnnotationType['rest']>,
  +typeParameters?: ?MaybeDetachedNode<
    HookTypeAnnotationType['typeParameters'],
  >,
};

export type IfStatementProps = {
  +test: MaybeDetachedNode<IfStatementType['test']>,
  +consequent: MaybeDetachedNode<IfStatementType['consequent']>,
  +alternate?: ?MaybeDetachedNode<IfStatementType['alternate']>,
};

export type ImportAttributeProps = {
  +key: MaybeDetachedNode<ImportAttributeType['key']>,
  +value: MaybeDetachedNode<ImportAttributeType['value']>,
};

export type ImportDeclarationProps = {
  +specifiers: $ReadOnlyArray<
    MaybeDetachedNode<ImportDeclarationType['specifiers'][number]>,
  >,
  +source: MaybeDetachedNode<ImportDeclarationType['source']>,
  +attributes?: ?$ReadOnlyArray<
    MaybeDetachedNode<ImportDeclarationType['attributes'][number]>,
  >,
  +importKind: ImportDeclarationType['importKind'],
};

export type ImportDefaultSpecifierProps = {
  +local: MaybeDetachedNode<ImportDefaultSpecifierType['local']>,
};

export type ImportExpressionProps = {
  +source: MaybeDetachedNode<ImportExpressionType['source']>,
  +options?: ?MaybeDetachedNode<ImportExpressionType['options']>,
};

export type ImportNamespaceSpecifierProps = {
  +local: MaybeDetachedNode<ImportNamespaceSpecifierType['local']>,
};

export type ImportSpecifierProps = {
  +imported: MaybeDetachedNode<ImportSpecifierType['imported']>,
  +local: MaybeDetachedNode<ImportSpecifierType['local']>,
  +importKind: ImportSpecifierType['importKind'],
};

export type IndexedAccessTypeProps = {
  +objectType: MaybeDetachedNode<IndexedAccessTypeType['objectType']>,
  +indexType: MaybeDetachedNode<IndexedAccessTypeType['indexType']>,
};

export type InferredPredicateProps = {};

export type InferTypeAnnotationProps = {
  +typeParameter: MaybeDetachedNode<InferTypeAnnotationType['typeParameter']>,
};

export type InterfaceDeclarationProps = {
  +id: MaybeDetachedNode<InterfaceDeclarationType['id']>,
  +typeParameters?: ?MaybeDetachedNode<
    InterfaceDeclarationType['typeParameters'],
  >,
  +extends: $ReadOnlyArray<
    MaybeDetachedNode<InterfaceDeclarationType['extends'][number]>,
  >,
  +body: MaybeDetachedNode<InterfaceDeclarationType['body']>,
};

export type InterfaceExtendsProps = {
  +id: MaybeDetachedNode<InterfaceExtendsType['id']>,
  +typeParameters?: ?MaybeDetachedNode<InterfaceExtendsType['typeParameters']>,
};

export type InterfaceTypeAnnotationProps = {
  +extends: $ReadOnlyArray<
    MaybeDetachedNode<InterfaceTypeAnnotationType['extends'][number]>,
  >,
  +body?: ?MaybeDetachedNode<InterfaceTypeAnnotationType['body']>,
};

export type IntersectionTypeAnnotationProps = {
  +types: $ReadOnlyArray<
    MaybeDetachedNode<IntersectionTypeAnnotationType['types'][number]>,
  >,
};

export type JSXAttributeProps = {
  +name: MaybeDetachedNode<JSXAttributeType['name']>,
  +value?: ?MaybeDetachedNode<JSXAttributeType['value']>,
};

export type JSXClosingElementProps = {
  +name: MaybeDetachedNode<JSXClosingElementType['name']>,
};

export type JSXClosingFragmentProps = {};

export type JSXElementProps = {
  +openingElement: MaybeDetachedNode<JSXElementType['openingElement']>,
  +children: $ReadOnlyArray<
    MaybeDetachedNode<JSXElementType['children'][number]>,
  >,
  +closingElement?: ?MaybeDetachedNode<JSXElementType['closingElement']>,
};

export type JSXEmptyExpressionProps = {};

export type JSXExpressionContainerProps = {
  +expression: MaybeDetachedNode<JSXExpressionContainerType['expression']>,
};

export type JSXFragmentProps = {
  +openingFragment: MaybeDetachedNode<JSXFragmentType['openingFragment']>,
  +children: $ReadOnlyArray<
    MaybeDetachedNode<JSXFragmentType['children'][number]>,
  >,
  +closingFragment: MaybeDetachedNode<JSXFragmentType['closingFragment']>,
};

export type JSXIdentifierProps = {
  +name: JSXIdentifierType['name'],
};

export type JSXMemberExpressionProps = {
  +object: MaybeDetachedNode<JSXMemberExpressionType['object']>,
  +property: MaybeDetachedNode<JSXMemberExpressionType['property']>,
};

export type JSXNamespacedNameProps = {
  +namespace: MaybeDetachedNode<JSXNamespacedNameType['namespace']>,
  +name: MaybeDetachedNode<JSXNamespacedNameType['name']>,
};

export type JSXOpeningElementProps = {
  +name: MaybeDetachedNode<JSXOpeningElementType['name']>,
  +attributes: $ReadOnlyArray<
    MaybeDetachedNode<JSXOpeningElementType['attributes'][number]>,
  >,
  +selfClosing: JSXOpeningElementType['selfClosing'],
  +typeArguments?: ?MaybeDetachedNode<JSXOpeningElementType['typeArguments']>,
};

export type JSXOpeningFragmentProps = {};

export type JSXSpreadAttributeProps = {
  +argument: MaybeDetachedNode<JSXSpreadAttributeType['argument']>,
};

export type JSXSpreadChildProps = {
  +expression: MaybeDetachedNode<JSXSpreadChildType['expression']>,
};

export type JSXTextProps = {
  +value: JSXTextType['value'],
  +raw: JSXTextType['raw'],
};

export type KeyofTypeAnnotationProps = {
  +argument: MaybeDetachedNode<KeyofTypeAnnotationType['argument']>,
};

export type LabeledStatementProps = {
  +label: MaybeDetachedNode<LabeledStatementType['label']>,
  +body: MaybeDetachedNode<LabeledStatementType['body']>,
};

export type LogicalExpressionProps = {
  +left: MaybeDetachedNode<LogicalExpressionType['left']>,
  +right: MaybeDetachedNode<LogicalExpressionType['right']>,
  +operator: LogicalExpressionType['operator'],
};

export type MatchArrayPatternProps = {
  +elements: $ReadOnlyArray<
    MaybeDetachedNode<MatchArrayPatternType['elements'][number]>,
  >,
  +rest?: ?MaybeDetachedNode<MatchArrayPatternType['rest']>,
};

export type MatchAsPatternProps = {
  +pattern: MaybeDetachedNode<MatchAsPatternType['pattern']>,
  +target: MaybeDetachedNode<MatchAsPatternType['target']>,
};

export type MatchBindingPatternProps = {
  +id: MaybeDetachedNode<MatchBindingPatternType['id']>,
  +kind: MatchBindingPatternType['kind'],
};

export type MatchExpressionProps = {
  +argument: MaybeDetachedNode<MatchExpressionType['argument']>,
  +cases: $ReadOnlyArray<
    MaybeDetachedNode<MatchExpressionType['cases'][number]>,
  >,
};

export type MatchExpressionCaseProps = {
  +pattern: MaybeDetachedNode<MatchExpressionCaseType['pattern']>,
  +body: MaybeDetachedNode<MatchExpressionCaseType['body']>,
  +guard?: ?MaybeDetachedNode<MatchExpressionCaseType['guard']>,
};

export type MatchIdentifierPatternProps = {
  +id: MaybeDetachedNode<MatchIdentifierPatternType['id']>,
};

export type MatchInstanceObjectPatternProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<MatchInstanceObjectPatternType['properties'][number]>,
  >,
  +rest?: ?MaybeDetachedNode<MatchInstanceObjectPatternType['rest']>,
};

export type MatchInstancePatternProps = {
  +targetConstructor: MaybeDetachedNode<
    MatchInstancePatternType['targetConstructor'],
  >,
  +properties: MaybeDetachedNode<MatchInstancePatternType['properties']>,
};

export type MatchLiteralPatternProps = {
  +literal: MaybeDetachedNode<MatchLiteralPatternType['literal']>,
};

export type MatchMemberPatternProps = {
  +base: MaybeDetachedNode<MatchMemberPatternType['base']>,
  +property: MaybeDetachedNode<MatchMemberPatternType['property']>,
};

export type MatchObjectPatternProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<MatchObjectPatternType['properties'][number]>,
  >,
  +rest?: ?MaybeDetachedNode<MatchObjectPatternType['rest']>,
};

export type MatchObjectPatternPropertyProps = {
  +key: MaybeDetachedNode<MatchObjectPatternPropertyType['key']>,
  +pattern: MaybeDetachedNode<MatchObjectPatternPropertyType['pattern']>,
  +shorthand: MatchObjectPatternPropertyType['shorthand'],
};

export type MatchOrPatternProps = {
  +patterns: $ReadOnlyArray<
    MaybeDetachedNode<MatchOrPatternType['patterns'][number]>,
  >,
};

export type MatchRestPatternProps = {
  +argument?: ?MaybeDetachedNode<MatchRestPatternType['argument']>,
};

export type MatchStatementProps = {
  +argument: MaybeDetachedNode<MatchStatementType['argument']>,
  +cases: $ReadOnlyArray<
    MaybeDetachedNode<MatchStatementType['cases'][number]>,
  >,
};

export type MatchStatementCaseProps = {
  +pattern: MaybeDetachedNode<MatchStatementCaseType['pattern']>,
  +body: MaybeDetachedNode<MatchStatementCaseType['body']>,
  +guard?: ?MaybeDetachedNode<MatchStatementCaseType['guard']>,
};

export type MatchUnaryPatternProps = {
  +argument: MaybeDetachedNode<MatchUnaryPatternType['argument']>,
  +operator: MatchUnaryPatternType['operator'],
};

export type MatchWildcardPatternProps = {};

export type MetaPropertyProps = {
  +meta: MaybeDetachedNode<MetaPropertyType['meta']>,
  +property: MaybeDetachedNode<MetaPropertyType['property']>,
};

export type MethodDefinitionProps = {
  +key: MaybeDetachedNode<MethodDefinitionType['key']>,
  +value: MaybeDetachedNode<MethodDefinitionType['value']>,
  +kind: MethodDefinitionType['kind'],
  +computed: MethodDefinitionType['computed'],
  +static: MethodDefinitionType['static'],
  +decorators: $ReadOnlyArray<
    MaybeDetachedNode<MethodDefinitionType['decorators'][number]>,
  >,
};

export type MixedTypeAnnotationProps = {};

export type NeverTypeAnnotationProps = {};

export type NewExpressionProps = {
  +callee: MaybeDetachedNode<NewExpressionType['callee']>,
  +typeArguments?: ?MaybeDetachedNode<NewExpressionType['typeArguments']>,
  +arguments: $ReadOnlyArray<
    MaybeDetachedNode<NewExpressionType['arguments'][number]>,
  >,
};

export type NullableTypeAnnotationProps = {
  +typeAnnotation: MaybeDetachedNode<
    NullableTypeAnnotationType['typeAnnotation'],
  >,
};

export type NullLiteralTypeAnnotationProps = {};

export type NumberLiteralTypeAnnotationProps = {
  +value: NumberLiteralTypeAnnotationType['value'],
  +raw: NumberLiteralTypeAnnotationType['raw'],
};

export type NumberTypeAnnotationProps = {};

export type ObjectExpressionProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<ObjectExpressionType['properties'][number]>,
  >,
};

export type ObjectPatternProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<ObjectPatternType['properties'][number]>,
  >,
  +typeAnnotation?: ?MaybeDetachedNode<ObjectPatternType['typeAnnotation']>,
};

export type ObjectTypeAnnotationProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<ObjectTypeAnnotationType['properties'][number]>,
  >,
  +indexers: $ReadOnlyArray<
    MaybeDetachedNode<ObjectTypeAnnotationType['indexers'][number]>,
  >,
  +callProperties: $ReadOnlyArray<
    MaybeDetachedNode<ObjectTypeAnnotationType['callProperties'][number]>,
  >,
  +internalSlots: $ReadOnlyArray<
    MaybeDetachedNode<ObjectTypeAnnotationType['internalSlots'][number]>,
  >,
  +inexact: ObjectTypeAnnotationType['inexact'],
  +exact: ObjectTypeAnnotationType['exact'],
};

export type ObjectTypeCallPropertyProps = {
  +value: MaybeDetachedNode<ObjectTypeCallPropertyType['value']>,
  +static: ObjectTypeCallPropertyType['static'],
};

export type ObjectTypeIndexerProps = {
  +id?: ?MaybeDetachedNode<ObjectTypeIndexerType['id']>,
  +key: MaybeDetachedNode<ObjectTypeIndexerType['key']>,
  +value: MaybeDetachedNode<ObjectTypeIndexerType['value']>,
  +static: ObjectTypeIndexerType['static'],
  +variance?: ?MaybeDetachedNode<ObjectTypeIndexerType['variance']>,
};

export type ObjectTypeInternalSlotProps = {
  +id: MaybeDetachedNode<ObjectTypeInternalSlotType['id']>,
  +value: MaybeDetachedNode<ObjectTypeInternalSlotType['value']>,
  +optional: ObjectTypeInternalSlotType['optional'],
  +static: ObjectTypeInternalSlotType['static'],
  +method: ObjectTypeInternalSlotType['method'],
};

export type ObjectTypeMappedTypePropertyProps = {
  +keyTparam: MaybeDetachedNode<ObjectTypeMappedTypePropertyType['keyTparam']>,
  +propType: MaybeDetachedNode<ObjectTypeMappedTypePropertyType['propType']>,
  +sourceType: MaybeDetachedNode<
    ObjectTypeMappedTypePropertyType['sourceType'],
  >,
  +variance?: ?MaybeDetachedNode<ObjectTypeMappedTypePropertyType['variance']>,
  +optional?: ?ObjectTypeMappedTypePropertyType['optional'],
};

export type ObjectTypeSpreadPropertyProps = {
  +argument: MaybeDetachedNode<ObjectTypeSpreadPropertyType['argument']>,
};

export type OpaqueTypeProps = {
  +id: MaybeDetachedNode<OpaqueTypeType['id']>,
  +typeParameters?: ?MaybeDetachedNode<OpaqueTypeType['typeParameters']>,
  +impltype: MaybeDetachedNode<OpaqueTypeType['impltype']>,
  +lowerBound?: ?MaybeDetachedNode<OpaqueTypeType['lowerBound']>,
  +upperBound?: ?MaybeDetachedNode<OpaqueTypeType['upperBound']>,
  +supertype?: ?MaybeDetachedNode<OpaqueTypeType['supertype']>,
};

export type OptionalIndexedAccessTypeProps = {
  +objectType: MaybeDetachedNode<OptionalIndexedAccessTypeType['objectType']>,
  +indexType: MaybeDetachedNode<OptionalIndexedAccessTypeType['indexType']>,
  +optional: OptionalIndexedAccessTypeType['optional'],
};

export type PrivateIdentifierProps = {
  +name: PrivateIdentifierType['name'],
};

export type PropertyProps = {
  +key: MaybeDetachedNode<PropertyType['key']>,
  +value: MaybeDetachedNode<PropertyType['value']>,
  +kind: PropertyType['kind'],
  +computed: PropertyType['computed'],
  +method: PropertyType['method'],
  +shorthand: PropertyType['shorthand'],
};

export type PropertyDefinitionProps = {
  +key: MaybeDetachedNode<PropertyDefinitionType['key']>,
  +value?: ?MaybeDetachedNode<PropertyDefinitionType['value']>,
  +computed: PropertyDefinitionType['computed'],
  +static: PropertyDefinitionType['static'],
  +decorators: $ReadOnlyArray<
    MaybeDetachedNode<PropertyDefinitionType['decorators'][number]>,
  >,
  +declare: PropertyDefinitionType['declare'],
  +optional: PropertyDefinitionType['optional'],
  +variance?: ?MaybeDetachedNode<PropertyDefinitionType['variance']>,
  +typeAnnotation?: ?MaybeDetachedNode<
    PropertyDefinitionType['typeAnnotation'],
  >,
};

export type QualifiedTypeIdentifierProps = {
  +qualification: MaybeDetachedNode<
    QualifiedTypeIdentifierType['qualification'],
  >,
  +id: MaybeDetachedNode<QualifiedTypeIdentifierType['id']>,
};

export type QualifiedTypeofIdentifierProps = {
  +qualification: MaybeDetachedNode<
    QualifiedTypeofIdentifierType['qualification'],
  >,
  +id: MaybeDetachedNode<QualifiedTypeofIdentifierType['id']>,
};

export type RecordDeclarationProps = {
  +id: MaybeDetachedNode<RecordDeclarationType['id']>,
  +typeParameters?: ?MaybeDetachedNode<RecordDeclarationType['typeParameters']>,
  +implements: $ReadOnlyArray<
    MaybeDetachedNode<RecordDeclarationType['implements'][number]>,
  >,
  +body: MaybeDetachedNode<RecordDeclarationType['body']>,
};

export type RecordDeclarationBodyProps = {
  +elements: $ReadOnlyArray<
    MaybeDetachedNode<RecordDeclarationBodyType['elements'][number]>,
  >,
};

export type RecordDeclarationImplementsProps = {
  +id: MaybeDetachedNode<RecordDeclarationImplementsType['id']>,
  +typeArguments?: ?MaybeDetachedNode<
    RecordDeclarationImplementsType['typeArguments'],
  >,
};

export type RecordDeclarationPropertyProps = {
  +key: MaybeDetachedNode<RecordDeclarationPropertyType['key']>,
  +typeAnnotation: MaybeDetachedNode<
    RecordDeclarationPropertyType['typeAnnotation'],
  >,
  +defaultValue?: ?MaybeDetachedNode<
    RecordDeclarationPropertyType['defaultValue'],
  >,
};

export type RecordDeclarationStaticPropertyProps = {
  +key: MaybeDetachedNode<RecordDeclarationStaticPropertyType['key']>,
  +typeAnnotation: MaybeDetachedNode<
    RecordDeclarationStaticPropertyType['typeAnnotation'],
  >,
  +value: MaybeDetachedNode<RecordDeclarationStaticPropertyType['value']>,
};

export type RecordExpressionProps = {
  +recordConstructor: MaybeDetachedNode<
    RecordExpressionType['recordConstructor'],
  >,
  +typeArguments?: ?MaybeDetachedNode<RecordExpressionType['typeArguments']>,
  +properties: MaybeDetachedNode<RecordExpressionType['properties']>,
};

export type RecordExpressionPropertiesProps = {
  +properties: $ReadOnlyArray<
    MaybeDetachedNode<RecordExpressionPropertiesType['properties'][number]>,
  >,
};

export type RestElementProps = {
  +argument: MaybeDetachedNode<RestElementType['argument']>,
};

export type ReturnStatementProps = {
  +argument?: ?MaybeDetachedNode<ReturnStatementType['argument']>,
};

export type SequenceExpressionProps = {
  +expressions: $ReadOnlyArray<
    MaybeDetachedNode<SequenceExpressionType['expressions'][number]>,
  >,
};

export type SpreadElementProps = {
  +argument: MaybeDetachedNode<SpreadElementType['argument']>,
};

export type StaticBlockProps = {
  +body: $ReadOnlyArray<MaybeDetachedNode<StaticBlockType['body'][number]>>,
};

export type StringLiteralTypeAnnotationProps = {
  +value: StringLiteralTypeAnnotationType['value'],
  +raw: StringLiteralTypeAnnotationType['raw'],
};

export type StringTypeAnnotationProps = {};

export type SuperProps = {};

export type SwitchCaseProps = {
  +test?: ?MaybeDetachedNode<SwitchCaseType['test']>,
  +consequent: $ReadOnlyArray<
    MaybeDetachedNode<SwitchCaseType['consequent'][number]>,
  >,
};

export type SwitchStatementProps = {
  +discriminant: MaybeDetachedNode<SwitchStatementType['discriminant']>,
  +cases: $ReadOnlyArray<
    MaybeDetachedNode<SwitchStatementType['cases'][number]>,
  >,
};

export type SymbolTypeAnnotationProps = {};

export type TaggedTemplateExpressionProps = {
  +tag: MaybeDetachedNode<TaggedTemplateExpressionType['tag']>,
  +quasi: MaybeDetachedNode<TaggedTemplateExpressionType['quasi']>,
};

export type TemplateLiteralProps = {
  +quasis: $ReadOnlyArray<
    MaybeDetachedNode<TemplateLiteralType['quasis'][number]>,
  >,
  +expressions: $ReadOnlyArray<
    MaybeDetachedNode<TemplateLiteralType['expressions'][number]>,
  >,
};

export type ThisExpressionProps = {};

export type ThisTypeAnnotationProps = {};

export type ThrowStatementProps = {
  +argument: MaybeDetachedNode<ThrowStatementType['argument']>,
};

export type TryStatementProps = {
  +block: MaybeDetachedNode<TryStatementType['block']>,
  +handler?: ?MaybeDetachedNode<TryStatementType['handler']>,
  +finalizer?: ?MaybeDetachedNode<TryStatementType['finalizer']>,
};

export type TupleTypeAnnotationProps = {
  +elementTypes: $ReadOnlyArray<
    MaybeDetachedNode<TupleTypeAnnotationType['elementTypes'][number]>,
  >,
  +inexact: TupleTypeAnnotationType['inexact'],
};

export type TupleTypeLabeledElementProps = {
  +label: MaybeDetachedNode<TupleTypeLabeledElementType['label']>,
  +elementType: MaybeDetachedNode<TupleTypeLabeledElementType['elementType']>,
  +optional: TupleTypeLabeledElementType['optional'],
  +variance?: ?MaybeDetachedNode<TupleTypeLabeledElementType['variance']>,
};

export type TupleTypeSpreadElementProps = {
  +label?: ?MaybeDetachedNode<TupleTypeSpreadElementType['label']>,
  +typeAnnotation: MaybeDetachedNode<
    TupleTypeSpreadElementType['typeAnnotation'],
  >,
};

export type TypeAliasProps = {
  +id: MaybeDetachedNode<TypeAliasType['id']>,
  +typeParameters?: ?MaybeDetachedNode<TypeAliasType['typeParameters']>,
  +right: MaybeDetachedNode<TypeAliasType['right']>,
};

export type TypeAnnotationProps = {
  +typeAnnotation: MaybeDetachedNode<TypeAnnotationType['typeAnnotation']>,
};

export type TypeCastExpressionProps = {
  +expression: MaybeDetachedNode<TypeCastExpressionType['expression']>,
  +typeAnnotation: MaybeDetachedNode<TypeCastExpressionType['typeAnnotation']>,
};

export type TypeofTypeAnnotationProps = {
  +argument: MaybeDetachedNode<TypeofTypeAnnotationType['argument']>,
  +typeArguments?: ?MaybeDetachedNode<
    TypeofTypeAnnotationType['typeArguments'],
  >,
};

export type TypeOperatorProps = {
  +operator: TypeOperatorType['operator'],
  +typeAnnotation: MaybeDetachedNode<TypeOperatorType['typeAnnotation']>,
};

export type TypeParameterProps = {
  +name: TypeParameterType['name'],
  +const: TypeParameterType['const'],
  +bound?: ?MaybeDetachedNode<TypeParameterType['bound']>,
  +variance?: ?MaybeDetachedNode<TypeParameterType['variance']>,
  +default?: ?MaybeDetachedNode<TypeParameterType['default']>,
  +usesExtendsBound: TypeParameterType['usesExtendsBound'],
};

export type TypeParameterDeclarationProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<TypeParameterDeclarationType['params'][number]>,
  >,
};

export type TypeParameterInstantiationProps = {
  +params: $ReadOnlyArray<
    MaybeDetachedNode<TypeParameterInstantiationType['params'][number]>,
  >,
};

export type TypePredicateProps = {
  +parameterName: MaybeDetachedNode<TypePredicateType['parameterName']>,
  +typeAnnotation?: ?MaybeDetachedNode<TypePredicateType['typeAnnotation']>,
  +kind?: ?TypePredicateType['kind'],
};

export type UnaryExpressionProps = {
  +operator: UnaryExpressionType['operator'],
  +argument: MaybeDetachedNode<UnaryExpressionType['argument']>,
  +prefix: UnaryExpressionType['prefix'],
};

export type UndefinedTypeAnnotationProps = {};

export type UnionTypeAnnotationProps = {
  +types: $ReadOnlyArray<
    MaybeDetachedNode<UnionTypeAnnotationType['types'][number]>,
  >,
};

export type UnknownTypeAnnotationProps = {};

export type UpdateExpressionProps = {
  +operator: UpdateExpressionType['operator'],
  +argument: MaybeDetachedNode<UpdateExpressionType['argument']>,
  +prefix: UpdateExpressionType['prefix'],
};

export type VariableDeclarationProps = {
  +kind: VariableDeclarationType['kind'],
  +declarations: $ReadOnlyArray<
    MaybeDetachedNode<VariableDeclarationType['declarations'][number]>,
  >,
};

export type VariableDeclaratorProps = {
  +init?: ?MaybeDetachedNode<VariableDeclaratorType['init']>,
  +id: MaybeDetachedNode<VariableDeclaratorType['id']>,
};

export type VarianceProps = {
  +kind: VarianceType['kind'],
};

export type VoidTypeAnnotationProps = {};

export type WhileStatementProps = {
  +body: MaybeDetachedNode<WhileStatementType['body']>,
  +test: MaybeDetachedNode<WhileStatementType['test']>,
};

export type WithStatementProps = {
  +object: MaybeDetachedNode<WithStatementType['object']>,
  +body: MaybeDetachedNode<WithStatementType['body']>,
};

export type YieldExpressionProps = {
  +argument?: ?MaybeDetachedNode<YieldExpressionType['argument']>,
  +delegate?: ?YieldExpressionType['delegate'],
};

export function AnyTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<AnyTypeAnnotationType> {
  return detachedProps<AnyTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'AnyTypeAnnotation',
  });
}

export function ArrayExpression(props: {
  ...ArrayExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ArrayExpressionType> {
  const node = detachedProps<ArrayExpressionType>((props.parent: $FlowFixMe), {
    type: 'ArrayExpression',
    elements: props.elements.map(n => asDetachedNodeForCodeGen(n)),
    trailingComma: props.trailingComma,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ArrayPattern(props: {
  ...ArrayPatternProps,
  +parent?: ESNode,
}): DetachedNode<ArrayPatternType> {
  const node = detachedProps<ArrayPatternType>((props.parent: $FlowFixMe), {
    type: 'ArrayPattern',
    elements: props.elements.map(n => asDetachedNodeForCodeGen(n)),
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ArrayTypeAnnotation(props: {
  ...ArrayTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<ArrayTypeAnnotationType> {
  const node = detachedProps<ArrayTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ArrayTypeAnnotation',
      elementType: asDetachedNodeForCodeGen(props.elementType),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function AsConstExpression(props: {
  ...AsConstExpressionProps,
  +parent?: ESNode,
}): DetachedNode<AsConstExpressionType> {
  const node = detachedProps<AsConstExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'AsConstExpression',
      expression: asDetachedNodeForCodeGen(props.expression),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function AsExpression(props: {
  ...AsExpressionProps,
  +parent?: ESNode,
}): DetachedNode<AsExpressionType> {
  const node = detachedProps<AsExpressionType>((props.parent: $FlowFixMe), {
    type: 'AsExpression',
    expression: asDetachedNodeForCodeGen(props.expression),
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function AssignmentExpression(props: {
  ...AssignmentExpressionProps,
  +parent?: ESNode,
}): DetachedNode<AssignmentExpressionType> {
  const node = detachedProps<AssignmentExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'AssignmentExpression',
      operator: props.operator,
      left: asDetachedNodeForCodeGen(props.left),
      right: asDetachedNodeForCodeGen(props.right),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function AssignmentPattern(props: {
  ...AssignmentPatternProps,
  +parent?: ESNode,
}): DetachedNode<AssignmentPatternType> {
  const node = detachedProps<AssignmentPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'AssignmentPattern',
      left: asDetachedNodeForCodeGen(props.left),
      right: asDetachedNodeForCodeGen(props.right),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function AwaitExpression(props: {
  ...AwaitExpressionProps,
  +parent?: ESNode,
}): DetachedNode<AwaitExpressionType> {
  const node = detachedProps<AwaitExpressionType>((props.parent: $FlowFixMe), {
    type: 'AwaitExpression',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function BigIntLiteralTypeAnnotation(props: {
  ...BigIntLiteralTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<BigIntLiteralTypeAnnotationType> {
  const node = detachedProps<BigIntLiteralTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'BigIntLiteralTypeAnnotation',
      raw: props.raw,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function BigIntTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<BigIntTypeAnnotationType> {
  return detachedProps<BigIntTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'BigIntTypeAnnotation',
  });
}

export function BinaryExpression(props: {
  ...BinaryExpressionProps,
  +parent?: ESNode,
}): DetachedNode<BinaryExpressionType> {
  const node = detachedProps<BinaryExpressionType>((props.parent: $FlowFixMe), {
    type: 'BinaryExpression',
    left: asDetachedNodeForCodeGen(props.left),
    right: asDetachedNodeForCodeGen(props.right),
    operator: props.operator,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function BooleanLiteralTypeAnnotation(props: {
  ...BooleanLiteralTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<BooleanLiteralTypeAnnotationType> {
  const node = detachedProps<BooleanLiteralTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'BooleanLiteralTypeAnnotation',
      value: props.value,
      raw: props.raw,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function BooleanTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<BooleanTypeAnnotationType> {
  return detachedProps<BooleanTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'BooleanTypeAnnotation',
  });
}

export function BreakStatement(props: {
  ...BreakStatementProps,
  +parent?: ESNode,
}): DetachedNode<BreakStatementType> {
  const node = detachedProps<BreakStatementType>((props.parent: $FlowFixMe), {
    type: 'BreakStatement',
    label: asDetachedNodeForCodeGen(props.label),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function CallExpression(props: {
  ...CallExpressionProps,
  +parent?: ESNode,
}): DetachedNode<CallExpressionType> {
  const node = detachedProps<CallExpressionType>((props.parent: $FlowFixMe), {
    type: 'CallExpression',
    callee: asDetachedNodeForCodeGen(props.callee),
    typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    arguments: props.arguments.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function CatchClause(props: {
  ...CatchClauseProps,
  +parent?: ESNode,
}): DetachedNode<CatchClauseType> {
  const node = detachedProps<CatchClauseType>((props.parent: $FlowFixMe), {
    type: 'CatchClause',
    param: asDetachedNodeForCodeGen(props.param),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ChainExpression(props: {
  ...ChainExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ChainExpressionType> {
  const node = detachedProps<ChainExpressionType>((props.parent: $FlowFixMe), {
    type: 'ChainExpression',
    expression: asDetachedNodeForCodeGen(props.expression),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ClassBody(props: {
  ...ClassBodyProps,
  +parent?: ESNode,
}): DetachedNode<ClassBodyType> {
  const node = detachedProps<ClassBodyType>((props.parent: $FlowFixMe), {
    type: 'ClassBody',
    body: props.body.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ClassExpression(props: {
  ...ClassExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ClassExpressionType> {
  const node = detachedProps<ClassExpressionType>((props.parent: $FlowFixMe), {
    type: 'ClassExpression',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    superClass: asDetachedNodeForCodeGen(props.superClass),
    superTypeArguments: asDetachedNodeForCodeGen(props.superTypeArguments),
    implements: props.implements.map(n => asDetachedNodeForCodeGen(n)),
    decorators: props.decorators.map(n => asDetachedNodeForCodeGen(n)),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ClassImplements(props: {
  ...ClassImplementsProps,
  +parent?: ESNode,
}): DetachedNode<ClassImplementsType> {
  const node = detachedProps<ClassImplementsType>((props.parent: $FlowFixMe), {
    type: 'ClassImplements',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ComponentDeclaration(props: {
  ...ComponentDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<ComponentDeclarationType> {
  const node = detachedProps<ComponentDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ComponentDeclaration',
      id: asDetachedNodeForCodeGen(props.id),
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      rendersType: asDetachedNodeForCodeGen(props.rendersType),
      async: props.async,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ComponentParameter(props: {
  ...ComponentParameterProps,
  +parent?: ESNode,
}): DetachedNode<ComponentParameterType> {
  const node = detachedProps<ComponentParameterType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ComponentParameter',
      name: asDetachedNodeForCodeGen(props.name),
      local: asDetachedNodeForCodeGen(props.local),
      shorthand: props.shorthand,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ComponentTypeAnnotation(props: {
  ...ComponentTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<ComponentTypeAnnotationType> {
  const node = detachedProps<ComponentTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ComponentTypeAnnotation',
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      rest: asDetachedNodeForCodeGen(props.rest),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      rendersType: asDetachedNodeForCodeGen(props.rendersType),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ComponentTypeParameter(props: {
  ...ComponentTypeParameterProps,
  +parent?: ESNode,
}): DetachedNode<ComponentTypeParameterType> {
  const node = detachedProps<ComponentTypeParameterType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ComponentTypeParameter',
      name: asDetachedNodeForCodeGen(props.name),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
      optional: props.optional,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ConditionalExpression(props: {
  ...ConditionalExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ConditionalExpressionType> {
  const node = detachedProps<ConditionalExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ConditionalExpression',
      test: asDetachedNodeForCodeGen(props.test),
      alternate: asDetachedNodeForCodeGen(props.alternate),
      consequent: asDetachedNodeForCodeGen(props.consequent),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ConditionalTypeAnnotation(props: {
  ...ConditionalTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<ConditionalTypeAnnotationType> {
  const node = detachedProps<ConditionalTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ConditionalTypeAnnotation',
      checkType: asDetachedNodeForCodeGen(props.checkType),
      extendsType: asDetachedNodeForCodeGen(props.extendsType),
      trueType: asDetachedNodeForCodeGen(props.trueType),
      falseType: asDetachedNodeForCodeGen(props.falseType),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ContinueStatement(props: {
  ...ContinueStatementProps,
  +parent?: ESNode,
}): DetachedNode<ContinueStatementType> {
  const node = detachedProps<ContinueStatementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ContinueStatement',
      label: asDetachedNodeForCodeGen(props.label),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DebuggerStatement(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<DebuggerStatementType> {
  return detachedProps<DebuggerStatementType>((props.parent: $FlowFixMe), {
    type: 'DebuggerStatement',
  });
}

export function DeclareClass(props: {
  ...DeclareClassProps,
  +parent?: ESNode,
}): DetachedNode<DeclareClassType> {
  const node = detachedProps<DeclareClassType>((props.parent: $FlowFixMe), {
    type: 'DeclareClass',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    extends: props.extends.map(n => asDetachedNodeForCodeGen(n)),
    implements: props.implements.map(n => asDetachedNodeForCodeGen(n)),
    mixins: props.mixins.map(n => asDetachedNodeForCodeGen(n)),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareComponent(props: {
  ...DeclareComponentProps,
  +parent?: ESNode,
}): DetachedNode<DeclareComponentType> {
  const node = detachedProps<DeclareComponentType>((props.parent: $FlowFixMe), {
    type: 'DeclareComponent',
    id: asDetachedNodeForCodeGen(props.id),
    params: props.params.map(n => asDetachedNodeForCodeGen(n)),
    rest: asDetachedNodeForCodeGen(props.rest),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    rendersType: asDetachedNodeForCodeGen(props.rendersType),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclaredPredicate(props: {
  ...DeclaredPredicateProps,
  +parent?: ESNode,
}): DetachedNode<DeclaredPredicateType> {
  const node = detachedProps<DeclaredPredicateType>(
    (props.parent: $FlowFixMe),
    {
      type: 'DeclaredPredicate',
      value: asDetachedNodeForCodeGen(props.value),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareEnum(props: {
  ...DeclareEnumProps,
  +parent?: ESNode,
}): DetachedNode<DeclareEnumType> {
  const node = detachedProps<DeclareEnumType>((props.parent: $FlowFixMe), {
    type: 'DeclareEnum',
    id: asDetachedNodeForCodeGen(props.id),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareExportAllDeclaration(props: {
  ...DeclareExportAllDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<DeclareExportAllDeclarationType> {
  const node = detachedProps<DeclareExportAllDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'DeclareExportAllDeclaration',
      source: asDetachedNodeForCodeGen(props.source),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareInterface(props: {
  ...DeclareInterfaceProps,
  +parent?: ESNode,
}): DetachedNode<DeclareInterfaceType> {
  const node = detachedProps<DeclareInterfaceType>((props.parent: $FlowFixMe), {
    type: 'DeclareInterface',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    extends: props.extends.map(n => asDetachedNodeForCodeGen(n)),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareModule(props: {
  ...DeclareModuleProps,
  +parent?: ESNode,
}): DetachedNode<DeclareModuleType> {
  const node = detachedProps<DeclareModuleType>((props.parent: $FlowFixMe), {
    type: 'DeclareModule',
    id: asDetachedNodeForCodeGen(props.id),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareModuleExports(props: {
  ...DeclareModuleExportsProps,
  +parent?: ESNode,
}): DetachedNode<DeclareModuleExportsType> {
  const node = detachedProps<DeclareModuleExportsType>(
    (props.parent: $FlowFixMe),
    {
      type: 'DeclareModuleExports',
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareNamespace(props: {
  ...DeclareNamespaceProps,
  +parent?: ESNode,
}): DetachedNode<DeclareNamespaceType> {
  const node = detachedProps<DeclareNamespaceType>((props.parent: $FlowFixMe), {
    type: 'DeclareNamespace',
    id: asDetachedNodeForCodeGen(props.id),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareOpaqueType(props: {
  ...DeclareOpaqueTypeProps,
  +parent?: ESNode,
}): DetachedNode<DeclareOpaqueTypeType> {
  const node = detachedProps<DeclareOpaqueTypeType>(
    (props.parent: $FlowFixMe),
    {
      type: 'DeclareOpaqueType',
      id: asDetachedNodeForCodeGen(props.id),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      impltype: asDetachedNodeForCodeGen(props.impltype),
      lowerBound: asDetachedNodeForCodeGen(props.lowerBound),
      upperBound: asDetachedNodeForCodeGen(props.upperBound),
      supertype: asDetachedNodeForCodeGen(props.supertype),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareTypeAlias(props: {
  ...DeclareTypeAliasProps,
  +parent?: ESNode,
}): DetachedNode<DeclareTypeAliasType> {
  const node = detachedProps<DeclareTypeAliasType>((props.parent: $FlowFixMe), {
    type: 'DeclareTypeAlias',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    right: asDetachedNodeForCodeGen(props.right),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DeclareVariable(props: {
  ...DeclareVariableProps,
  +parent?: ESNode,
}): DetachedNode<DeclareVariableType> {
  const node = detachedProps<DeclareVariableType>((props.parent: $FlowFixMe), {
    type: 'DeclareVariable',
    id: asDetachedNodeForCodeGen(props.id),
    kind: props.kind,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function Decorator(props: {
  ...DecoratorProps,
  +parent?: ESNode,
}): DetachedNode<DecoratorType> {
  const node = detachedProps<DecoratorType>((props.parent: $FlowFixMe), {
    type: 'Decorator',
    expression: asDetachedNodeForCodeGen(props.expression),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function DoWhileStatement(props: {
  ...DoWhileStatementProps,
  +parent?: ESNode,
}): DetachedNode<DoWhileStatementType> {
  const node = detachedProps<DoWhileStatementType>((props.parent: $FlowFixMe), {
    type: 'DoWhileStatement',
    body: asDetachedNodeForCodeGen(props.body),
    test: asDetachedNodeForCodeGen(props.test),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EmptyStatement(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<EmptyStatementType> {
  return detachedProps<EmptyStatementType>((props.parent: $FlowFixMe), {
    type: 'EmptyStatement',
  });
}

export function EmptyTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<EmptyTypeAnnotationType> {
  return detachedProps<EmptyTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'EmptyTypeAnnotation',
  });
}

export function EnumBigIntBody(props: {
  ...EnumBigIntBodyProps,
  +parent?: ESNode,
}): DetachedNode<EnumBigIntBodyType> {
  const node = detachedProps<EnumBigIntBodyType>((props.parent: $FlowFixMe), {
    type: 'EnumBigIntBody',
    members: props.members.map(n => asDetachedNodeForCodeGen(n)),
    explicitType: props.explicitType,
    hasUnknownMembers: props.hasUnknownMembers,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumBigIntMember(props: {
  ...EnumBigIntMemberProps,
  +parent?: ESNode,
}): DetachedNode<EnumBigIntMemberType> {
  const node = detachedProps<EnumBigIntMemberType>((props.parent: $FlowFixMe), {
    type: 'EnumBigIntMember',
    id: asDetachedNodeForCodeGen(props.id),
    init: asDetachedNodeForCodeGen(props.init),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumBooleanBody(props: {
  ...EnumBooleanBodyProps,
  +parent?: ESNode,
}): DetachedNode<EnumBooleanBodyType> {
  const node = detachedProps<EnumBooleanBodyType>((props.parent: $FlowFixMe), {
    type: 'EnumBooleanBody',
    members: props.members.map(n => asDetachedNodeForCodeGen(n)),
    explicitType: props.explicitType,
    hasUnknownMembers: props.hasUnknownMembers,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumBooleanMember(props: {
  ...EnumBooleanMemberProps,
  +parent?: ESNode,
}): DetachedNode<EnumBooleanMemberType> {
  const node = detachedProps<EnumBooleanMemberType>(
    (props.parent: $FlowFixMe),
    {
      type: 'EnumBooleanMember',
      id: asDetachedNodeForCodeGen(props.id),
      init: asDetachedNodeForCodeGen(props.init),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumDeclaration(props: {
  ...EnumDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<EnumDeclarationType> {
  const node = detachedProps<EnumDeclarationType>((props.parent: $FlowFixMe), {
    type: 'EnumDeclaration',
    id: asDetachedNodeForCodeGen(props.id),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumDefaultedMember(props: {
  ...EnumDefaultedMemberProps,
  +parent?: ESNode,
}): DetachedNode<EnumDefaultedMemberType> {
  const node = detachedProps<EnumDefaultedMemberType>(
    (props.parent: $FlowFixMe),
    {
      type: 'EnumDefaultedMember',
      id: asDetachedNodeForCodeGen(props.id),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumNumberBody(props: {
  ...EnumNumberBodyProps,
  +parent?: ESNode,
}): DetachedNode<EnumNumberBodyType> {
  const node = detachedProps<EnumNumberBodyType>((props.parent: $FlowFixMe), {
    type: 'EnumNumberBody',
    members: props.members.map(n => asDetachedNodeForCodeGen(n)),
    explicitType: props.explicitType,
    hasUnknownMembers: props.hasUnknownMembers,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumNumberMember(props: {
  ...EnumNumberMemberProps,
  +parent?: ESNode,
}): DetachedNode<EnumNumberMemberType> {
  const node = detachedProps<EnumNumberMemberType>((props.parent: $FlowFixMe), {
    type: 'EnumNumberMember',
    id: asDetachedNodeForCodeGen(props.id),
    init: asDetachedNodeForCodeGen(props.init),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumStringBody(props: {
  ...EnumStringBodyProps,
  +parent?: ESNode,
}): DetachedNode<EnumStringBodyType> {
  const node = detachedProps<EnumStringBodyType>((props.parent: $FlowFixMe), {
    type: 'EnumStringBody',
    members: props.members.map(n => asDetachedNodeForCodeGen(n)),
    explicitType: props.explicitType,
    hasUnknownMembers: props.hasUnknownMembers,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumStringMember(props: {
  ...EnumStringMemberProps,
  +parent?: ESNode,
}): DetachedNode<EnumStringMemberType> {
  const node = detachedProps<EnumStringMemberType>((props.parent: $FlowFixMe), {
    type: 'EnumStringMember',
    id: asDetachedNodeForCodeGen(props.id),
    init: asDetachedNodeForCodeGen(props.init),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function EnumSymbolBody(props: {
  ...EnumSymbolBodyProps,
  +parent?: ESNode,
}): DetachedNode<EnumSymbolBodyType> {
  const node = detachedProps<EnumSymbolBodyType>((props.parent: $FlowFixMe), {
    type: 'EnumSymbolBody',
    members: props.members.map(n => asDetachedNodeForCodeGen(n)),
    hasUnknownMembers: props.hasUnknownMembers,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ExistsTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<ExistsTypeAnnotationType> {
  return detachedProps<ExistsTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'ExistsTypeAnnotation',
  });
}

export function ExportAllDeclaration(props: {
  ...ExportAllDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<ExportAllDeclarationType> {
  const node = detachedProps<ExportAllDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ExportAllDeclaration',
      exported: asDetachedNodeForCodeGen(props.exported),
      source: asDetachedNodeForCodeGen(props.source),
      exportKind: props.exportKind,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ExportDefaultDeclaration(props: {
  ...ExportDefaultDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<ExportDefaultDeclarationType> {
  const node = detachedProps<ExportDefaultDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ExportDefaultDeclaration',
      declaration: asDetachedNodeForCodeGen(props.declaration),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ExportSpecifier(props: {
  ...ExportSpecifierProps,
  +parent?: ESNode,
}): DetachedNode<ExportSpecifierType> {
  const node = detachedProps<ExportSpecifierType>((props.parent: $FlowFixMe), {
    type: 'ExportSpecifier',
    exported: asDetachedNodeForCodeGen(props.exported),
    local: asDetachedNodeForCodeGen(props.local),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ExpressionStatement(props: {
  ...ExpressionStatementProps,
  +parent?: ESNode,
}): DetachedNode<ExpressionStatementType> {
  const node = detachedProps<ExpressionStatementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ExpressionStatement',
      expression: asDetachedNodeForCodeGen(props.expression),
      directive: props.directive,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ForInStatement(props: {
  ...ForInStatementProps,
  +parent?: ESNode,
}): DetachedNode<ForInStatementType> {
  const node = detachedProps<ForInStatementType>((props.parent: $FlowFixMe), {
    type: 'ForInStatement',
    left: asDetachedNodeForCodeGen(props.left),
    right: asDetachedNodeForCodeGen(props.right),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ForOfStatement(props: {
  ...ForOfStatementProps,
  +parent?: ESNode,
}): DetachedNode<ForOfStatementType> {
  const node = detachedProps<ForOfStatementType>((props.parent: $FlowFixMe), {
    type: 'ForOfStatement',
    left: asDetachedNodeForCodeGen(props.left),
    right: asDetachedNodeForCodeGen(props.right),
    body: asDetachedNodeForCodeGen(props.body),
    await: props.await,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ForStatement(props: {
  ...ForStatementProps,
  +parent?: ESNode,
}): DetachedNode<ForStatementType> {
  const node = detachedProps<ForStatementType>((props.parent: $FlowFixMe), {
    type: 'ForStatement',
    init: asDetachedNodeForCodeGen(props.init),
    test: asDetachedNodeForCodeGen(props.test),
    update: asDetachedNodeForCodeGen(props.update),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function FunctionDeclaration(props: {
  ...FunctionDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<FunctionDeclarationType> {
  const node = detachedProps<FunctionDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'FunctionDeclaration',
      id: asDetachedNodeForCodeGen(props.id),
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      returnType: asDetachedNodeForCodeGen(props.returnType),
      predicate: asDetachedNodeForCodeGen(props.predicate),
      generator: props.generator,
      async: props.async,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function FunctionExpression(props: {
  ...FunctionExpressionProps,
  +parent?: ESNode,
}): DetachedNode<FunctionExpressionType> {
  const node = detachedProps<FunctionExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'FunctionExpression',
      id: asDetachedNodeForCodeGen(props.id),
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      returnType: asDetachedNodeForCodeGen(props.returnType),
      predicate: asDetachedNodeForCodeGen(props.predicate),
      generator: props.generator,
      async: props.async,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function FunctionTypeAnnotation(props: {
  ...FunctionTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<FunctionTypeAnnotationType> {
  const node = detachedProps<FunctionTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'FunctionTypeAnnotation',
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      this: asDetachedNodeForCodeGen(props.this),
      returnType: asDetachedNodeForCodeGen(props.returnType),
      rest: asDetachedNodeForCodeGen(props.rest),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function FunctionTypeParam(props: {
  ...FunctionTypeParamProps,
  +parent?: ESNode,
}): DetachedNode<FunctionTypeParamType> {
  const node = detachedProps<FunctionTypeParamType>(
    (props.parent: $FlowFixMe),
    {
      type: 'FunctionTypeParam',
      name: asDetachedNodeForCodeGen(props.name),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
      optional: props.optional,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function GenericTypeAnnotation(props: {
  ...GenericTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<GenericTypeAnnotationType> {
  const node = detachedProps<GenericTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'GenericTypeAnnotation',
      id: asDetachedNodeForCodeGen(props.id),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function HookDeclaration(props: {
  ...HookDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<HookDeclarationType> {
  const node = detachedProps<HookDeclarationType>((props.parent: $FlowFixMe), {
    type: 'HookDeclaration',
    id: asDetachedNodeForCodeGen(props.id),
    params: props.params.map(n => asDetachedNodeForCodeGen(n)),
    body: asDetachedNodeForCodeGen(props.body),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    returnType: asDetachedNodeForCodeGen(props.returnType),
    async: props.async,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function HookTypeAnnotation(props: {
  ...HookTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<HookTypeAnnotationType> {
  const node = detachedProps<HookTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'HookTypeAnnotation',
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
      returnType: asDetachedNodeForCodeGen(props.returnType),
      rest: asDetachedNodeForCodeGen(props.rest),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function IfStatement(props: {
  ...IfStatementProps,
  +parent?: ESNode,
}): DetachedNode<IfStatementType> {
  const node = detachedProps<IfStatementType>((props.parent: $FlowFixMe), {
    type: 'IfStatement',
    test: asDetachedNodeForCodeGen(props.test),
    consequent: asDetachedNodeForCodeGen(props.consequent),
    alternate: asDetachedNodeForCodeGen(props.alternate),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportAttribute(props: {
  ...ImportAttributeProps,
  +parent?: ESNode,
}): DetachedNode<ImportAttributeType> {
  const node = detachedProps<ImportAttributeType>((props.parent: $FlowFixMe), {
    type: 'ImportAttribute',
    key: asDetachedNodeForCodeGen(props.key),
    value: asDetachedNodeForCodeGen(props.value),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportDeclaration(props: {
  ...ImportDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<ImportDeclarationType> {
  const node = detachedProps<ImportDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ImportDeclaration',
      specifiers: props.specifiers.map(n => asDetachedNodeForCodeGen(n)),
      source: asDetachedNodeForCodeGen(props.source),
      attributes: props.attributes?.map(n => asDetachedNodeForCodeGen(n)),
      importKind: props.importKind,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportDefaultSpecifier(props: {
  ...ImportDefaultSpecifierProps,
  +parent?: ESNode,
}): DetachedNode<ImportDefaultSpecifierType> {
  const node = detachedProps<ImportDefaultSpecifierType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ImportDefaultSpecifier',
      local: asDetachedNodeForCodeGen(props.local),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportExpression(props: {
  ...ImportExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ImportExpressionType> {
  const node = detachedProps<ImportExpressionType>((props.parent: $FlowFixMe), {
    type: 'ImportExpression',
    source: asDetachedNodeForCodeGen(props.source),
    options: asDetachedNodeForCodeGen(props.options),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportNamespaceSpecifier(props: {
  ...ImportNamespaceSpecifierProps,
  +parent?: ESNode,
}): DetachedNode<ImportNamespaceSpecifierType> {
  const node = detachedProps<ImportNamespaceSpecifierType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ImportNamespaceSpecifier',
      local: asDetachedNodeForCodeGen(props.local),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ImportSpecifier(props: {
  ...ImportSpecifierProps,
  +parent?: ESNode,
}): DetachedNode<ImportSpecifierType> {
  const node = detachedProps<ImportSpecifierType>((props.parent: $FlowFixMe), {
    type: 'ImportSpecifier',
    imported: asDetachedNodeForCodeGen(props.imported),
    local: asDetachedNodeForCodeGen(props.local),
    importKind: props.importKind,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function IndexedAccessType(props: {
  ...IndexedAccessTypeProps,
  +parent?: ESNode,
}): DetachedNode<IndexedAccessTypeType> {
  const node = detachedProps<IndexedAccessTypeType>(
    (props.parent: $FlowFixMe),
    {
      type: 'IndexedAccessType',
      objectType: asDetachedNodeForCodeGen(props.objectType),
      indexType: asDetachedNodeForCodeGen(props.indexType),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function InferredPredicate(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<InferredPredicateType> {
  return detachedProps<InferredPredicateType>((props.parent: $FlowFixMe), {
    type: 'InferredPredicate',
  });
}

export function InferTypeAnnotation(props: {
  ...InferTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<InferTypeAnnotationType> {
  const node = detachedProps<InferTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'InferTypeAnnotation',
      typeParameter: asDetachedNodeForCodeGen(props.typeParameter),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function InterfaceDeclaration(props: {
  ...InterfaceDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<InterfaceDeclarationType> {
  const node = detachedProps<InterfaceDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'InterfaceDeclaration',
      id: asDetachedNodeForCodeGen(props.id),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      extends: props.extends.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function InterfaceExtends(props: {
  ...InterfaceExtendsProps,
  +parent?: ESNode,
}): DetachedNode<InterfaceExtendsType> {
  const node = detachedProps<InterfaceExtendsType>((props.parent: $FlowFixMe), {
    type: 'InterfaceExtends',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function InterfaceTypeAnnotation(props: {
  ...InterfaceTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<InterfaceTypeAnnotationType> {
  const node = detachedProps<InterfaceTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'InterfaceTypeAnnotation',
      extends: props.extends.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function IntersectionTypeAnnotation(props: {
  ...IntersectionTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<IntersectionTypeAnnotationType> {
  const node = detachedProps<IntersectionTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'IntersectionTypeAnnotation',
      types: props.types.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXAttribute(props: {
  ...JSXAttributeProps,
  +parent?: ESNode,
}): DetachedNode<JSXAttributeType> {
  const node = detachedProps<JSXAttributeType>((props.parent: $FlowFixMe), {
    type: 'JSXAttribute',
    name: asDetachedNodeForCodeGen(props.name),
    value: asDetachedNodeForCodeGen(props.value),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXClosingElement(props: {
  ...JSXClosingElementProps,
  +parent?: ESNode,
}): DetachedNode<JSXClosingElementType> {
  const node = detachedProps<JSXClosingElementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXClosingElement',
      name: asDetachedNodeForCodeGen(props.name),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXClosingFragment(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<JSXClosingFragmentType> {
  return detachedProps<JSXClosingFragmentType>((props.parent: $FlowFixMe), {
    type: 'JSXClosingFragment',
  });
}

export function JSXElement(props: {
  ...JSXElementProps,
  +parent?: ESNode,
}): DetachedNode<JSXElementType> {
  const node = detachedProps<JSXElementType>((props.parent: $FlowFixMe), {
    type: 'JSXElement',
    openingElement: asDetachedNodeForCodeGen(props.openingElement),
    children: props.children.map(n => asDetachedNodeForCodeGen(n)),
    closingElement: asDetachedNodeForCodeGen(props.closingElement),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXEmptyExpression(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<JSXEmptyExpressionType> {
  return detachedProps<JSXEmptyExpressionType>((props.parent: $FlowFixMe), {
    type: 'JSXEmptyExpression',
  });
}

export function JSXExpressionContainer(props: {
  ...JSXExpressionContainerProps,
  +parent?: ESNode,
}): DetachedNode<JSXExpressionContainerType> {
  const node = detachedProps<JSXExpressionContainerType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXExpressionContainer',
      expression: asDetachedNodeForCodeGen(props.expression),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXFragment(props: {
  ...JSXFragmentProps,
  +parent?: ESNode,
}): DetachedNode<JSXFragmentType> {
  const node = detachedProps<JSXFragmentType>((props.parent: $FlowFixMe), {
    type: 'JSXFragment',
    openingFragment: asDetachedNodeForCodeGen(props.openingFragment),
    children: props.children.map(n => asDetachedNodeForCodeGen(n)),
    closingFragment: asDetachedNodeForCodeGen(props.closingFragment),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXIdentifier(props: {
  ...JSXIdentifierProps,
  +parent?: ESNode,
}): DetachedNode<JSXIdentifierType> {
  const node = detachedProps<JSXIdentifierType>((props.parent: $FlowFixMe), {
    type: 'JSXIdentifier',
    name: props.name,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXMemberExpression(props: {
  ...JSXMemberExpressionProps,
  +parent?: ESNode,
}): DetachedNode<JSXMemberExpressionType> {
  const node = detachedProps<JSXMemberExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXMemberExpression',
      object: asDetachedNodeForCodeGen(props.object),
      property: asDetachedNodeForCodeGen(props.property),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXNamespacedName(props: {
  ...JSXNamespacedNameProps,
  +parent?: ESNode,
}): DetachedNode<JSXNamespacedNameType> {
  const node = detachedProps<JSXNamespacedNameType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXNamespacedName',
      namespace: asDetachedNodeForCodeGen(props.namespace),
      name: asDetachedNodeForCodeGen(props.name),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXOpeningElement(props: {
  ...JSXOpeningElementProps,
  +parent?: ESNode,
}): DetachedNode<JSXOpeningElementType> {
  const node = detachedProps<JSXOpeningElementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXOpeningElement',
      name: asDetachedNodeForCodeGen(props.name),
      attributes: props.attributes.map(n => asDetachedNodeForCodeGen(n)),
      selfClosing: props.selfClosing,
      typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXOpeningFragment(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<JSXOpeningFragmentType> {
  return detachedProps<JSXOpeningFragmentType>((props.parent: $FlowFixMe), {
    type: 'JSXOpeningFragment',
  });
}

export function JSXSpreadAttribute(props: {
  ...JSXSpreadAttributeProps,
  +parent?: ESNode,
}): DetachedNode<JSXSpreadAttributeType> {
  const node = detachedProps<JSXSpreadAttributeType>(
    (props.parent: $FlowFixMe),
    {
      type: 'JSXSpreadAttribute',
      argument: asDetachedNodeForCodeGen(props.argument),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXSpreadChild(props: {
  ...JSXSpreadChildProps,
  +parent?: ESNode,
}): DetachedNode<JSXSpreadChildType> {
  const node = detachedProps<JSXSpreadChildType>((props.parent: $FlowFixMe), {
    type: 'JSXSpreadChild',
    expression: asDetachedNodeForCodeGen(props.expression),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function JSXText(props: {
  ...JSXTextProps,
  +parent?: ESNode,
}): DetachedNode<JSXTextType> {
  const node = detachedProps<JSXTextType>((props.parent: $FlowFixMe), {
    type: 'JSXText',
    value: props.value,
    raw: props.raw,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function KeyofTypeAnnotation(props: {
  ...KeyofTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<KeyofTypeAnnotationType> {
  const node = detachedProps<KeyofTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'KeyofTypeAnnotation',
      argument: asDetachedNodeForCodeGen(props.argument),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function LabeledStatement(props: {
  ...LabeledStatementProps,
  +parent?: ESNode,
}): DetachedNode<LabeledStatementType> {
  const node = detachedProps<LabeledStatementType>((props.parent: $FlowFixMe), {
    type: 'LabeledStatement',
    label: asDetachedNodeForCodeGen(props.label),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function LogicalExpression(props: {
  ...LogicalExpressionProps,
  +parent?: ESNode,
}): DetachedNode<LogicalExpressionType> {
  const node = detachedProps<LogicalExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'LogicalExpression',
      left: asDetachedNodeForCodeGen(props.left),
      right: asDetachedNodeForCodeGen(props.right),
      operator: props.operator,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchArrayPattern(props: {
  ...MatchArrayPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchArrayPatternType> {
  const node = detachedProps<MatchArrayPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchArrayPattern',
      elements: props.elements.map(n => asDetachedNodeForCodeGen(n)),
      rest: asDetachedNodeForCodeGen(props.rest),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchAsPattern(props: {
  ...MatchAsPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchAsPatternType> {
  const node = detachedProps<MatchAsPatternType>((props.parent: $FlowFixMe), {
    type: 'MatchAsPattern',
    pattern: asDetachedNodeForCodeGen(props.pattern),
    target: asDetachedNodeForCodeGen(props.target),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchBindingPattern(props: {
  ...MatchBindingPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchBindingPatternType> {
  const node = detachedProps<MatchBindingPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchBindingPattern',
      id: asDetachedNodeForCodeGen(props.id),
      kind: props.kind,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchExpression(props: {
  ...MatchExpressionProps,
  +parent?: ESNode,
}): DetachedNode<MatchExpressionType> {
  const node = detachedProps<MatchExpressionType>((props.parent: $FlowFixMe), {
    type: 'MatchExpression',
    argument: asDetachedNodeForCodeGen(props.argument),
    cases: props.cases.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchExpressionCase(props: {
  ...MatchExpressionCaseProps,
  +parent?: ESNode,
}): DetachedNode<MatchExpressionCaseType> {
  const node = detachedProps<MatchExpressionCaseType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchExpressionCase',
      pattern: asDetachedNodeForCodeGen(props.pattern),
      body: asDetachedNodeForCodeGen(props.body),
      guard: asDetachedNodeForCodeGen(props.guard),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchIdentifierPattern(props: {
  ...MatchIdentifierPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchIdentifierPatternType> {
  const node = detachedProps<MatchIdentifierPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchIdentifierPattern',
      id: asDetachedNodeForCodeGen(props.id),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchInstanceObjectPattern(props: {
  ...MatchInstanceObjectPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchInstanceObjectPatternType> {
  const node = detachedProps<MatchInstanceObjectPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchInstanceObjectPattern',
      properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
      rest: asDetachedNodeForCodeGen(props.rest),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchInstancePattern(props: {
  ...MatchInstancePatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchInstancePatternType> {
  const node = detachedProps<MatchInstancePatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchInstancePattern',
      targetConstructor: asDetachedNodeForCodeGen(props.targetConstructor),
      properties: asDetachedNodeForCodeGen(props.properties),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchLiteralPattern(props: {
  ...MatchLiteralPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchLiteralPatternType> {
  const node = detachedProps<MatchLiteralPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchLiteralPattern',
      literal: asDetachedNodeForCodeGen(props.literal),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchMemberPattern(props: {
  ...MatchMemberPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchMemberPatternType> {
  const node = detachedProps<MatchMemberPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchMemberPattern',
      base: asDetachedNodeForCodeGen(props.base),
      property: asDetachedNodeForCodeGen(props.property),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchObjectPattern(props: {
  ...MatchObjectPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchObjectPatternType> {
  const node = detachedProps<MatchObjectPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchObjectPattern',
      properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
      rest: asDetachedNodeForCodeGen(props.rest),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchObjectPatternProperty(props: {
  ...MatchObjectPatternPropertyProps,
  +parent?: ESNode,
}): DetachedNode<MatchObjectPatternPropertyType> {
  const node = detachedProps<MatchObjectPatternPropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchObjectPatternProperty',
      key: asDetachedNodeForCodeGen(props.key),
      pattern: asDetachedNodeForCodeGen(props.pattern),
      shorthand: props.shorthand,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchOrPattern(props: {
  ...MatchOrPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchOrPatternType> {
  const node = detachedProps<MatchOrPatternType>((props.parent: $FlowFixMe), {
    type: 'MatchOrPattern',
    patterns: props.patterns.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchRestPattern(props: {
  ...MatchRestPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchRestPatternType> {
  const node = detachedProps<MatchRestPatternType>((props.parent: $FlowFixMe), {
    type: 'MatchRestPattern',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchStatement(props: {
  ...MatchStatementProps,
  +parent?: ESNode,
}): DetachedNode<MatchStatementType> {
  const node = detachedProps<MatchStatementType>((props.parent: $FlowFixMe), {
    type: 'MatchStatement',
    argument: asDetachedNodeForCodeGen(props.argument),
    cases: props.cases.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchStatementCase(props: {
  ...MatchStatementCaseProps,
  +parent?: ESNode,
}): DetachedNode<MatchStatementCaseType> {
  const node = detachedProps<MatchStatementCaseType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchStatementCase',
      pattern: asDetachedNodeForCodeGen(props.pattern),
      body: asDetachedNodeForCodeGen(props.body),
      guard: asDetachedNodeForCodeGen(props.guard),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchUnaryPattern(props: {
  ...MatchUnaryPatternProps,
  +parent?: ESNode,
}): DetachedNode<MatchUnaryPatternType> {
  const node = detachedProps<MatchUnaryPatternType>(
    (props.parent: $FlowFixMe),
    {
      type: 'MatchUnaryPattern',
      argument: asDetachedNodeForCodeGen(props.argument),
      operator: props.operator,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MatchWildcardPattern(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<MatchWildcardPatternType> {
  return detachedProps<MatchWildcardPatternType>((props.parent: $FlowFixMe), {
    type: 'MatchWildcardPattern',
  });
}

export function MetaProperty(props: {
  ...MetaPropertyProps,
  +parent?: ESNode,
}): DetachedNode<MetaPropertyType> {
  const node = detachedProps<MetaPropertyType>((props.parent: $FlowFixMe), {
    type: 'MetaProperty',
    meta: asDetachedNodeForCodeGen(props.meta),
    property: asDetachedNodeForCodeGen(props.property),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MethodDefinition(props: {
  ...MethodDefinitionProps,
  +parent?: ESNode,
}): DetachedNode<MethodDefinitionType> {
  const node = detachedProps<MethodDefinitionType>((props.parent: $FlowFixMe), {
    type: 'MethodDefinition',
    key: asDetachedNodeForCodeGen(props.key),
    value: asDetachedNodeForCodeGen(props.value),
    kind: props.kind,
    computed: props.computed,
    static: props.static,
    decorators: props.decorators.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function MixedTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<MixedTypeAnnotationType> {
  return detachedProps<MixedTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'MixedTypeAnnotation',
  });
}

export function NeverTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<NeverTypeAnnotationType> {
  return detachedProps<NeverTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'NeverTypeAnnotation',
  });
}

export function NewExpression(props: {
  ...NewExpressionProps,
  +parent?: ESNode,
}): DetachedNode<NewExpressionType> {
  const node = detachedProps<NewExpressionType>((props.parent: $FlowFixMe), {
    type: 'NewExpression',
    callee: asDetachedNodeForCodeGen(props.callee),
    typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    arguments: props.arguments.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function NullableTypeAnnotation(props: {
  ...NullableTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<NullableTypeAnnotationType> {
  const node = detachedProps<NullableTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'NullableTypeAnnotation',
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function NullLiteralTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<NullLiteralTypeAnnotationType> {
  return detachedProps<NullLiteralTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'NullLiteralTypeAnnotation',
    },
  );
}

export function NumberLiteralTypeAnnotation(props: {
  ...NumberLiteralTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<NumberLiteralTypeAnnotationType> {
  const node = detachedProps<NumberLiteralTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'NumberLiteralTypeAnnotation',
      value: props.value,
      raw: props.raw,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function NumberTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<NumberTypeAnnotationType> {
  return detachedProps<NumberTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'NumberTypeAnnotation',
  });
}

export function ObjectExpression(props: {
  ...ObjectExpressionProps,
  +parent?: ESNode,
}): DetachedNode<ObjectExpressionType> {
  const node = detachedProps<ObjectExpressionType>((props.parent: $FlowFixMe), {
    type: 'ObjectExpression',
    properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectPattern(props: {
  ...ObjectPatternProps,
  +parent?: ESNode,
}): DetachedNode<ObjectPatternType> {
  const node = detachedProps<ObjectPatternType>((props.parent: $FlowFixMe), {
    type: 'ObjectPattern',
    properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeAnnotation(props: {
  ...ObjectTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeAnnotationType> {
  const node = detachedProps<ObjectTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeAnnotation',
      properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
      indexers: props.indexers.map(n => asDetachedNodeForCodeGen(n)),
      callProperties: props.callProperties.map(n =>
        asDetachedNodeForCodeGen(n),
      ),
      internalSlots: props.internalSlots.map(n => asDetachedNodeForCodeGen(n)),
      inexact: props.inexact,
      exact: props.exact,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeCallProperty(props: {
  ...ObjectTypeCallPropertyProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeCallPropertyType> {
  const node = detachedProps<ObjectTypeCallPropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeCallProperty',
      value: asDetachedNodeForCodeGen(props.value),
      static: props.static,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeIndexer(props: {
  ...ObjectTypeIndexerProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeIndexerType> {
  const node = detachedProps<ObjectTypeIndexerType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeIndexer',
      id: asDetachedNodeForCodeGen(props.id),
      key: asDetachedNodeForCodeGen(props.key),
      value: asDetachedNodeForCodeGen(props.value),
      static: props.static,
      variance: asDetachedNodeForCodeGen(props.variance),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeInternalSlot(props: {
  ...ObjectTypeInternalSlotProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeInternalSlotType> {
  const node = detachedProps<ObjectTypeInternalSlotType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeInternalSlot',
      id: asDetachedNodeForCodeGen(props.id),
      value: asDetachedNodeForCodeGen(props.value),
      optional: props.optional,
      static: props.static,
      method: props.method,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeMappedTypeProperty(props: {
  ...ObjectTypeMappedTypePropertyProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeMappedTypePropertyType> {
  const node = detachedProps<ObjectTypeMappedTypePropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeMappedTypeProperty',
      keyTparam: asDetachedNodeForCodeGen(props.keyTparam),
      propType: asDetachedNodeForCodeGen(props.propType),
      sourceType: asDetachedNodeForCodeGen(props.sourceType),
      variance: asDetachedNodeForCodeGen(props.variance),
      optional: props.optional,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ObjectTypeSpreadProperty(props: {
  ...ObjectTypeSpreadPropertyProps,
  +parent?: ESNode,
}): DetachedNode<ObjectTypeSpreadPropertyType> {
  const node = detachedProps<ObjectTypeSpreadPropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'ObjectTypeSpreadProperty',
      argument: asDetachedNodeForCodeGen(props.argument),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function OpaqueType(props: {
  ...OpaqueTypeProps,
  +parent?: ESNode,
}): DetachedNode<OpaqueTypeType> {
  const node = detachedProps<OpaqueTypeType>((props.parent: $FlowFixMe), {
    type: 'OpaqueType',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    impltype: asDetachedNodeForCodeGen(props.impltype),
    lowerBound: asDetachedNodeForCodeGen(props.lowerBound),
    upperBound: asDetachedNodeForCodeGen(props.upperBound),
    supertype: asDetachedNodeForCodeGen(props.supertype),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function OptionalIndexedAccessType(props: {
  ...OptionalIndexedAccessTypeProps,
  +parent?: ESNode,
}): DetachedNode<OptionalIndexedAccessTypeType> {
  const node = detachedProps<OptionalIndexedAccessTypeType>(
    (props.parent: $FlowFixMe),
    {
      type: 'OptionalIndexedAccessType',
      objectType: asDetachedNodeForCodeGen(props.objectType),
      indexType: asDetachedNodeForCodeGen(props.indexType),
      optional: props.optional,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function PrivateIdentifier(props: {
  ...PrivateIdentifierProps,
  +parent?: ESNode,
}): DetachedNode<PrivateIdentifierType> {
  const node = detachedProps<PrivateIdentifierType>(
    (props.parent: $FlowFixMe),
    {
      type: 'PrivateIdentifier',
      name: props.name,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function Property(props: {
  ...PropertyProps,
  +parent?: ESNode,
}): DetachedNode<PropertyType> {
  const node = detachedProps<PropertyType>((props.parent: $FlowFixMe), {
    type: 'Property',
    key: asDetachedNodeForCodeGen(props.key),
    value: asDetachedNodeForCodeGen(props.value),
    kind: props.kind,
    computed: props.computed,
    method: props.method,
    shorthand: props.shorthand,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function PropertyDefinition(props: {
  ...PropertyDefinitionProps,
  +parent?: ESNode,
}): DetachedNode<PropertyDefinitionType> {
  const node = detachedProps<PropertyDefinitionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'PropertyDefinition',
      key: asDetachedNodeForCodeGen(props.key),
      value: asDetachedNodeForCodeGen(props.value),
      computed: props.computed,
      static: props.static,
      decorators: props.decorators.map(n => asDetachedNodeForCodeGen(n)),
      declare: props.declare,
      optional: props.optional,
      variance: asDetachedNodeForCodeGen(props.variance),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function QualifiedTypeIdentifier(props: {
  ...QualifiedTypeIdentifierProps,
  +parent?: ESNode,
}): DetachedNode<QualifiedTypeIdentifierType> {
  const node = detachedProps<QualifiedTypeIdentifierType>(
    (props.parent: $FlowFixMe),
    {
      type: 'QualifiedTypeIdentifier',
      qualification: asDetachedNodeForCodeGen(props.qualification),
      id: asDetachedNodeForCodeGen(props.id),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function QualifiedTypeofIdentifier(props: {
  ...QualifiedTypeofIdentifierProps,
  +parent?: ESNode,
}): DetachedNode<QualifiedTypeofIdentifierType> {
  const node = detachedProps<QualifiedTypeofIdentifierType>(
    (props.parent: $FlowFixMe),
    {
      type: 'QualifiedTypeofIdentifier',
      qualification: asDetachedNodeForCodeGen(props.qualification),
      id: asDetachedNodeForCodeGen(props.id),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordDeclaration(props: {
  ...RecordDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<RecordDeclarationType> {
  const node = detachedProps<RecordDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordDeclaration',
      id: asDetachedNodeForCodeGen(props.id),
      typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
      implements: props.implements.map(n => asDetachedNodeForCodeGen(n)),
      body: asDetachedNodeForCodeGen(props.body),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordDeclarationBody(props: {
  ...RecordDeclarationBodyProps,
  +parent?: ESNode,
}): DetachedNode<RecordDeclarationBodyType> {
  const node = detachedProps<RecordDeclarationBodyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordDeclarationBody',
      elements: props.elements.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordDeclarationImplements(props: {
  ...RecordDeclarationImplementsProps,
  +parent?: ESNode,
}): DetachedNode<RecordDeclarationImplementsType> {
  const node = detachedProps<RecordDeclarationImplementsType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordDeclarationImplements',
      id: asDetachedNodeForCodeGen(props.id),
      typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordDeclarationProperty(props: {
  ...RecordDeclarationPropertyProps,
  +parent?: ESNode,
}): DetachedNode<RecordDeclarationPropertyType> {
  const node = detachedProps<RecordDeclarationPropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordDeclarationProperty',
      key: asDetachedNodeForCodeGen(props.key),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
      defaultValue: asDetachedNodeForCodeGen(props.defaultValue),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordDeclarationStaticProperty(props: {
  ...RecordDeclarationStaticPropertyProps,
  +parent?: ESNode,
}): DetachedNode<RecordDeclarationStaticPropertyType> {
  const node = detachedProps<RecordDeclarationStaticPropertyType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordDeclarationStaticProperty',
      key: asDetachedNodeForCodeGen(props.key),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
      value: asDetachedNodeForCodeGen(props.value),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordExpression(props: {
  ...RecordExpressionProps,
  +parent?: ESNode,
}): DetachedNode<RecordExpressionType> {
  const node = detachedProps<RecordExpressionType>((props.parent: $FlowFixMe), {
    type: 'RecordExpression',
    recordConstructor: asDetachedNodeForCodeGen(props.recordConstructor),
    typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    properties: asDetachedNodeForCodeGen(props.properties),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RecordExpressionProperties(props: {
  ...RecordExpressionPropertiesProps,
  +parent?: ESNode,
}): DetachedNode<RecordExpressionPropertiesType> {
  const node = detachedProps<RecordExpressionPropertiesType>(
    (props.parent: $FlowFixMe),
    {
      type: 'RecordExpressionProperties',
      properties: props.properties.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function RestElement(props: {
  ...RestElementProps,
  +parent?: ESNode,
}): DetachedNode<RestElementType> {
  const node = detachedProps<RestElementType>((props.parent: $FlowFixMe), {
    type: 'RestElement',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ReturnStatement(props: {
  ...ReturnStatementProps,
  +parent?: ESNode,
}): DetachedNode<ReturnStatementType> {
  const node = detachedProps<ReturnStatementType>((props.parent: $FlowFixMe), {
    type: 'ReturnStatement',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function SequenceExpression(props: {
  ...SequenceExpressionProps,
  +parent?: ESNode,
}): DetachedNode<SequenceExpressionType> {
  const node = detachedProps<SequenceExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'SequenceExpression',
      expressions: props.expressions.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function SpreadElement(props: {
  ...SpreadElementProps,
  +parent?: ESNode,
}): DetachedNode<SpreadElementType> {
  const node = detachedProps<SpreadElementType>((props.parent: $FlowFixMe), {
    type: 'SpreadElement',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function StaticBlock(props: {
  ...StaticBlockProps,
  +parent?: ESNode,
}): DetachedNode<StaticBlockType> {
  const node = detachedProps<StaticBlockType>((props.parent: $FlowFixMe), {
    type: 'StaticBlock',
    body: props.body.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function StringLiteralTypeAnnotation(props: {
  ...StringLiteralTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<StringLiteralTypeAnnotationType> {
  const node = detachedProps<StringLiteralTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'StringLiteralTypeAnnotation',
      value: props.value,
      raw: props.raw,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function StringTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<StringTypeAnnotationType> {
  return detachedProps<StringTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'StringTypeAnnotation',
  });
}

export function Super(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<SuperType> {
  return detachedProps<SuperType>((props.parent: $FlowFixMe), {
    type: 'Super',
  });
}

export function SwitchCase(props: {
  ...SwitchCaseProps,
  +parent?: ESNode,
}): DetachedNode<SwitchCaseType> {
  const node = detachedProps<SwitchCaseType>((props.parent: $FlowFixMe), {
    type: 'SwitchCase',
    test: asDetachedNodeForCodeGen(props.test),
    consequent: props.consequent.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function SwitchStatement(props: {
  ...SwitchStatementProps,
  +parent?: ESNode,
}): DetachedNode<SwitchStatementType> {
  const node = detachedProps<SwitchStatementType>((props.parent: $FlowFixMe), {
    type: 'SwitchStatement',
    discriminant: asDetachedNodeForCodeGen(props.discriminant),
    cases: props.cases.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function SymbolTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<SymbolTypeAnnotationType> {
  return detachedProps<SymbolTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'SymbolTypeAnnotation',
  });
}

export function TaggedTemplateExpression(props: {
  ...TaggedTemplateExpressionProps,
  +parent?: ESNode,
}): DetachedNode<TaggedTemplateExpressionType> {
  const node = detachedProps<TaggedTemplateExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TaggedTemplateExpression',
      tag: asDetachedNodeForCodeGen(props.tag),
      quasi: asDetachedNodeForCodeGen(props.quasi),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TemplateLiteral(props: {
  ...TemplateLiteralProps,
  +parent?: ESNode,
}): DetachedNode<TemplateLiteralType> {
  const node = detachedProps<TemplateLiteralType>((props.parent: $FlowFixMe), {
    type: 'TemplateLiteral',
    quasis: props.quasis.map(n => asDetachedNodeForCodeGen(n)),
    expressions: props.expressions.map(n => asDetachedNodeForCodeGen(n)),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function ThisExpression(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<ThisExpressionType> {
  return detachedProps<ThisExpressionType>((props.parent: $FlowFixMe), {
    type: 'ThisExpression',
  });
}

export function ThisTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<ThisTypeAnnotationType> {
  return detachedProps<ThisTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'ThisTypeAnnotation',
  });
}

export function ThrowStatement(props: {
  ...ThrowStatementProps,
  +parent?: ESNode,
}): DetachedNode<ThrowStatementType> {
  const node = detachedProps<ThrowStatementType>((props.parent: $FlowFixMe), {
    type: 'ThrowStatement',
    argument: asDetachedNodeForCodeGen(props.argument),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TryStatement(props: {
  ...TryStatementProps,
  +parent?: ESNode,
}): DetachedNode<TryStatementType> {
  const node = detachedProps<TryStatementType>((props.parent: $FlowFixMe), {
    type: 'TryStatement',
    block: asDetachedNodeForCodeGen(props.block),
    handler: asDetachedNodeForCodeGen(props.handler),
    finalizer: asDetachedNodeForCodeGen(props.finalizer),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TupleTypeAnnotation(props: {
  ...TupleTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<TupleTypeAnnotationType> {
  const node = detachedProps<TupleTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TupleTypeAnnotation',
      elementTypes: props.elementTypes.map(n => asDetachedNodeForCodeGen(n)),
      inexact: props.inexact,
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TupleTypeLabeledElement(props: {
  ...TupleTypeLabeledElementProps,
  +parent?: ESNode,
}): DetachedNode<TupleTypeLabeledElementType> {
  const node = detachedProps<TupleTypeLabeledElementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TupleTypeLabeledElement',
      label: asDetachedNodeForCodeGen(props.label),
      elementType: asDetachedNodeForCodeGen(props.elementType),
      optional: props.optional,
      variance: asDetachedNodeForCodeGen(props.variance),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TupleTypeSpreadElement(props: {
  ...TupleTypeSpreadElementProps,
  +parent?: ESNode,
}): DetachedNode<TupleTypeSpreadElementType> {
  const node = detachedProps<TupleTypeSpreadElementType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TupleTypeSpreadElement',
      label: asDetachedNodeForCodeGen(props.label),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeAlias(props: {
  ...TypeAliasProps,
  +parent?: ESNode,
}): DetachedNode<TypeAliasType> {
  const node = detachedProps<TypeAliasType>((props.parent: $FlowFixMe), {
    type: 'TypeAlias',
    id: asDetachedNodeForCodeGen(props.id),
    typeParameters: asDetachedNodeForCodeGen(props.typeParameters),
    right: asDetachedNodeForCodeGen(props.right),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeAnnotation(props: {
  ...TypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<TypeAnnotationType> {
  const node = detachedProps<TypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'TypeAnnotation',
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeCastExpression(props: {
  ...TypeCastExpressionProps,
  +parent?: ESNode,
}): DetachedNode<TypeCastExpressionType> {
  const node = detachedProps<TypeCastExpressionType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TypeCastExpression',
      expression: asDetachedNodeForCodeGen(props.expression),
      typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeofTypeAnnotation(props: {
  ...TypeofTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<TypeofTypeAnnotationType> {
  const node = detachedProps<TypeofTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TypeofTypeAnnotation',
      argument: asDetachedNodeForCodeGen(props.argument),
      typeArguments: asDetachedNodeForCodeGen(props.typeArguments),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeOperator(props: {
  ...TypeOperatorProps,
  +parent?: ESNode,
}): DetachedNode<TypeOperatorType> {
  const node = detachedProps<TypeOperatorType>((props.parent: $FlowFixMe), {
    type: 'TypeOperator',
    operator: props.operator,
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeParameter(props: {
  ...TypeParameterProps,
  +parent?: ESNode,
}): DetachedNode<TypeParameterType> {
  const node = detachedProps<TypeParameterType>((props.parent: $FlowFixMe), {
    type: 'TypeParameter',
    name: props.name,
    const: props.const,
    bound: asDetachedNodeForCodeGen(props.bound),
    variance: asDetachedNodeForCodeGen(props.variance),
    default: asDetachedNodeForCodeGen(props.default),
    usesExtendsBound: props.usesExtendsBound,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeParameterDeclaration(props: {
  ...TypeParameterDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<TypeParameterDeclarationType> {
  const node = detachedProps<TypeParameterDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TypeParameterDeclaration',
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypeParameterInstantiation(props: {
  ...TypeParameterInstantiationProps,
  +parent?: ESNode,
}): DetachedNode<TypeParameterInstantiationType> {
  const node = detachedProps<TypeParameterInstantiationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'TypeParameterInstantiation',
      params: props.params.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function TypePredicate(props: {
  ...TypePredicateProps,
  +parent?: ESNode,
}): DetachedNode<TypePredicateType> {
  const node = detachedProps<TypePredicateType>((props.parent: $FlowFixMe), {
    type: 'TypePredicate',
    parameterName: asDetachedNodeForCodeGen(props.parameterName),
    typeAnnotation: asDetachedNodeForCodeGen(props.typeAnnotation),
    kind: props.kind,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function UnaryExpression(props: {
  ...UnaryExpressionProps,
  +parent?: ESNode,
}): DetachedNode<UnaryExpressionType> {
  const node = detachedProps<UnaryExpressionType>((props.parent: $FlowFixMe), {
    type: 'UnaryExpression',
    operator: props.operator,
    argument: asDetachedNodeForCodeGen(props.argument),
    prefix: props.prefix,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function UndefinedTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<UndefinedTypeAnnotationType> {
  return detachedProps<UndefinedTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'UndefinedTypeAnnotation',
    },
  );
}

export function UnionTypeAnnotation(props: {
  ...UnionTypeAnnotationProps,
  +parent?: ESNode,
}): DetachedNode<UnionTypeAnnotationType> {
  const node = detachedProps<UnionTypeAnnotationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'UnionTypeAnnotation',
      types: props.types.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function UnknownTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<UnknownTypeAnnotationType> {
  return detachedProps<UnknownTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'UnknownTypeAnnotation',
  });
}

export function UpdateExpression(props: {
  ...UpdateExpressionProps,
  +parent?: ESNode,
}): DetachedNode<UpdateExpressionType> {
  const node = detachedProps<UpdateExpressionType>((props.parent: $FlowFixMe), {
    type: 'UpdateExpression',
    operator: props.operator,
    argument: asDetachedNodeForCodeGen(props.argument),
    prefix: props.prefix,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function VariableDeclaration(props: {
  ...VariableDeclarationProps,
  +parent?: ESNode,
}): DetachedNode<VariableDeclarationType> {
  const node = detachedProps<VariableDeclarationType>(
    (props.parent: $FlowFixMe),
    {
      type: 'VariableDeclaration',
      kind: props.kind,
      declarations: props.declarations.map(n => asDetachedNodeForCodeGen(n)),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function VariableDeclarator(props: {
  ...VariableDeclaratorProps,
  +parent?: ESNode,
}): DetachedNode<VariableDeclaratorType> {
  const node = detachedProps<VariableDeclaratorType>(
    (props.parent: $FlowFixMe),
    {
      type: 'VariableDeclarator',
      init: asDetachedNodeForCodeGen(props.init),
      id: asDetachedNodeForCodeGen(props.id),
    },
  );
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function Variance(props: {
  ...VarianceProps,
  +parent?: ESNode,
}): DetachedNode<VarianceType> {
  const node = detachedProps<VarianceType>((props.parent: $FlowFixMe), {
    type: 'Variance',
    kind: props.kind,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function VoidTypeAnnotation(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<VoidTypeAnnotationType> {
  return detachedProps<VoidTypeAnnotationType>((props.parent: $FlowFixMe), {
    type: 'VoidTypeAnnotation',
  });
}

export function WhileStatement(props: {
  ...WhileStatementProps,
  +parent?: ESNode,
}): DetachedNode<WhileStatementType> {
  const node = detachedProps<WhileStatementType>((props.parent: $FlowFixMe), {
    type: 'WhileStatement',
    body: asDetachedNodeForCodeGen(props.body),
    test: asDetachedNodeForCodeGen(props.test),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function WithStatement(props: {
  ...WithStatementProps,
  +parent?: ESNode,
}): DetachedNode<WithStatementType> {
  const node = detachedProps<WithStatementType>((props.parent: $FlowFixMe), {
    type: 'WithStatement',
    object: asDetachedNodeForCodeGen(props.object),
    body: asDetachedNodeForCodeGen(props.body),
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export function YieldExpression(props: {
  ...YieldExpressionProps,
  +parent?: ESNode,
}): DetachedNode<YieldExpressionType> {
  const node = detachedProps<YieldExpressionType>((props.parent: $FlowFixMe), {
    type: 'YieldExpression',
    argument: asDetachedNodeForCodeGen(props.argument),
    delegate: props.delegate,
  });
  setParentPointersInDirectChildren((node: $FlowFixMe));
  return node;
}

export * from './special-case-node-types';
