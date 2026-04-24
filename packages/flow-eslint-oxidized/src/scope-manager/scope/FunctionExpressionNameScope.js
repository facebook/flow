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

import type {FunctionExpression} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';
import {FunctionNameDefinition} from '../definition';

class FunctionExpressionNameScope extends ScopeBase<
  typeof ScopeType.FunctionExpressionName,
  FunctionExpression,
  Scope,
> {
  declare +type: typeof ScopeType.FunctionExpressionName;

  +functionExpressionScope: true = true;

  constructor(
    scopeManager: ScopeManager,
    upperScope: FunctionExpressionNameScope['upper'],
    block: FunctionExpressionNameScope['block'],
  ) {
    super(
      scopeManager,
      ScopeType.FunctionExpressionName,
      upperScope,
      block,
      false,
    );
    if (block.id) {
      this.defineIdentifier(
        block.id,
        new FunctionNameDefinition(block.id, block),
      );
    }
  }
}

export {FunctionExpressionNameScope};
