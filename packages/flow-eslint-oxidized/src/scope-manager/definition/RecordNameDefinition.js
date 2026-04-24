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

import type {RecordDeclaration, Identifier} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class RecordNameDefinition extends DefinitionBase<
  typeof DefinitionType.RecordName,
  RecordDeclaration,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.RecordName;

  constructor(name: Identifier, node: RecordNameDefinition['node']) {
    super(DefinitionType.RecordName, name, node, null);
  }

  +isTypeDefinition: boolean = true;
  +isVariableDefinition: boolean = true;
}

export {RecordNameDefinition};
