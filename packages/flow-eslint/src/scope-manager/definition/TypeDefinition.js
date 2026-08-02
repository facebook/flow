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
  DeclareTypeAlias,
  DeclareOpaqueType,
  DeclareInterface,
  TypeAlias,
  OpaqueType,
  InterfaceDeclaration,
  Identifier,
} from 'flow-estree';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class TypeDefinition extends DefinitionBase<
  typeof DefinitionType.Type,
  | DeclareTypeAlias
  | DeclareOpaqueType
  | DeclareInterface
  | TypeAlias
  | OpaqueType
  | InterfaceDeclaration,
  null,
  Identifier,
> {
  declare readonly type: typeof DefinitionType.Type;

  constructor(name: Identifier, node: TypeDefinition['node']) {
    super(DefinitionType.Type, name, node, null);
  }

  readonly isTypeDefinition: boolean = true;
  readonly isVariableDefinition: boolean = false;
}

export {TypeDefinition};
