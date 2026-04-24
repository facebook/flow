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
  HookDeclaration,
  DeclareHook,
  Identifier,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class HookNameDefinition extends DefinitionBase<
  typeof DefinitionType.HookName,
  HookDeclaration | DeclareHook,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.HookName;

  constructor(name: Identifier, node: HookNameDefinition['node']) {
    super(DefinitionType.HookName, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {HookNameDefinition};
