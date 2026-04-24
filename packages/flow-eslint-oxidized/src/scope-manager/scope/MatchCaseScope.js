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
  MatchExpressionCase,
  MatchStatementCase,
} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class MatchCaseScope extends ScopeBase<
  typeof ScopeType.MatchCase,
  MatchExpressionCase | MatchStatementCase,
  Scope,
> {
  declare +type: typeof ScopeType.MatchCase;

  constructor(
    scopeManager: ScopeManager,
    upperScope: MatchCaseScope['upper'],
    block: MatchCaseScope['block'],
  ) {
    super(scopeManager, ScopeType.MatchCase, upperScope, block, false);
  }
}

export {MatchCaseScope};
