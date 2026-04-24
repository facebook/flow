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

import type {CatchClause} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class CatchScope extends ScopeBase<typeof ScopeType.Catch, CatchClause, Scope> {
  declare +type: typeof ScopeType.Catch;

  constructor(
    scopeManager: ScopeManager,
    upperScope: CatchScope['upper'],
    block: CatchScope['block'],
  ) {
    super(scopeManager, ScopeType.Catch, upperScope, block, false);
  }
}

export {CatchScope};
