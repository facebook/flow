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
  ClassDeclaration,
  ClassExpression,
  DeclareClass,
  Identifier,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class ClassNameDefinition extends DefinitionBase<
  typeof DefinitionType.ClassName,
  ClassDeclaration | ClassExpression | DeclareClass,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.ClassName;

  constructor(name: Identifier, node: ClassNameDefinition['node']) {
    super(DefinitionType.ClassName, name, node, null);
  }

  +isTypeDefinition: boolean = true;
  +isVariableDefinition: boolean = true;
}

export {ClassNameDefinition};
