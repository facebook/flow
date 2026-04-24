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

import type {BlockStatement} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class BlockScope extends ScopeBase<
  typeof ScopeType.Block,
  BlockStatement,
  Scope,
> {
  declare +type: typeof ScopeType.Block;

  constructor(
    scopeManager: ScopeManager,
    upperScope: BlockScope['upper'],
    block: BlockScope['block'],
  ) {
    super(scopeManager, ScopeType.Block, upperScope, block, false);
  }
}

export {BlockScope};
