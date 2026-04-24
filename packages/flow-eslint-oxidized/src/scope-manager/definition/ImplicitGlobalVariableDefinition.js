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

import type {ESNode, BindingName} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class ImplicitGlobalVariableDefinition extends DefinitionBase<
  typeof DefinitionType.ImplicitGlobalVariable,
  ESNode,
  null,
  BindingName,
> {
  declare +type: typeof DefinitionType.ImplicitGlobalVariable;

  constructor(
    name: BindingName,
    node: ImplicitGlobalVariableDefinition['node'],
  ) {
    super(DefinitionType.ImplicitGlobalVariable, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {ImplicitGlobalVariableDefinition};
