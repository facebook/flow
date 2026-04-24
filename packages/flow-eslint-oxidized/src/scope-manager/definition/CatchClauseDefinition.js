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

import type {CatchClause, BindingName} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class CatchClauseDefinition extends DefinitionBase<
  typeof DefinitionType.CatchClause,
  CatchClause,
  null,
  BindingName,
> {
  declare +type: typeof DefinitionType.CatchClause;

  constructor(name: BindingName, node: CatchClauseDefinition['node']) {
    super(DefinitionType.CatchClause, name, node, null);
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {CatchClauseDefinition};
