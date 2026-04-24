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
  ForInStatement,
  ForOfStatement,
  ForStatement,
} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class ForScope extends ScopeBase<
  typeof ScopeType.For,
  ForInStatement | ForOfStatement | ForStatement,
  Scope,
> {
  declare +type: typeof ScopeType.For;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ForScope['upper'],
    block: ForScope['block'],
  ) {
    super(scopeManager, ScopeType.For, upperScope, block, false);
  }
}

export {ForScope};
