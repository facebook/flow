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
  ComponentDeclaration,
  DeclareComponent,
  Identifier,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class ComponentNameDefinition extends DefinitionBase<
  typeof DefinitionType.ComponentName,
  ComponentDeclaration | DeclareComponent,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.ComponentName;

  constructor(name: Identifier, node: ComponentNameDefinition['node']) {
    super(DefinitionType.ComponentName, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {ComponentNameDefinition};
