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

import type {DeclareNamespace, Identifier} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class NamespaceNameDefinition extends DefinitionBase<
  typeof DefinitionType.NamespaceName,
  DeclareNamespace,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.NamespaceName;

  constructor(name: Identifier, node: NamespaceNameDefinition['node']) {
    super(DefinitionType.NamespaceName, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {NamespaceNameDefinition};
