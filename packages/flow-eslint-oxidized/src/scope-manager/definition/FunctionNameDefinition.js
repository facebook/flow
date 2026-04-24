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
  DeclareFunction,
  FunctionDeclaration,
  FunctionExpression,
  Identifier,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class FunctionNameDefinition extends DefinitionBase<
  typeof DefinitionType.FunctionName,
  FunctionDeclaration | FunctionExpression | DeclareFunction,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.FunctionName;

  constructor(name: Identifier, node: FunctionNameDefinition['node']) {
    super(DefinitionType.FunctionName, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {FunctionNameDefinition};
