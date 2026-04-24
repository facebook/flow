/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {
  ComponentTypeAnnotation,
  FunctionDeclaration,
  FunctionExpression,
  ArrowFunctionExpression,
  DeclareTypeAlias,
  DeclareOpaqueType,
  DeclareInterface,
  DeclareClass,
  DeclareComponent,
  DeclareHook,
  FunctionTypeAnnotation,
  HookTypeAnnotation,
  TypeAlias,
  ObjectTypeMappedTypeProperty,
  OpaqueType,
  InterfaceDeclaration,
} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class TypeScope extends ScopeBase<
  typeof ScopeType.Type,
  | ComponentTypeAnnotation
  | FunctionDeclaration
  | FunctionExpression
  | ArrowFunctionExpression
  | DeclareTypeAlias
  | DeclareOpaqueType
  | DeclareInterface
  | DeclareClass
  | DeclareComponent
  | DeclareHook
  | FunctionTypeAnnotation
  | HookTypeAnnotation
  | TypeAlias
  | ObjectTypeMappedTypeProperty
  | OpaqueType
  | InterfaceDeclaration,
  Scope,
> {
  declare +type: typeof ScopeType.Type;

  constructor(
    scopeManager: ScopeManager,
    upperScope: TypeScope['upper'],
    block: TypeScope['block'],
  ) {
    super(scopeManager, ScopeType.Type, upperScope, block, false);
  }
}

export {TypeScope};
