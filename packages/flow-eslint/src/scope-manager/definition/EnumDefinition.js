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

import type {EnumDeclaration, Identifier, DeclareEnum} from 'flow-estree';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class EnumDefinition extends DefinitionBase<
  typeof DefinitionType.Enum,
  DeclareEnum | EnumDeclaration,
  null,
  Identifier,
> {
  declare readonly type: typeof DefinitionType.Enum;

  constructor(name: Identifier, node: EnumDefinition['node']) {
    super(DefinitionType.Enum, name, node, null);
  }

  readonly isTypeDefinition: boolean = true;
  readonly isVariableDefinition: boolean = true;
}

export {EnumDefinition};
