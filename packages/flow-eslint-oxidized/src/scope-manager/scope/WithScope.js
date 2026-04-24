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

import type {WithStatement} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class WithScope extends ScopeBase<typeof ScopeType.With, WithStatement, Scope> {
  declare +type: typeof ScopeType.With;

  constructor(
    scopeManager: ScopeManager,
    upperScope: WithScope['upper'],
    block: WithScope['block'],
  ) {
    super(scopeManager, ScopeType.With, upperScope, block, false);
  }
  close(scopeManager: ScopeManager): Scope | null {
    if (this.shouldStaticallyClose()) {
      return super.close(scopeManager);
    }
    if (this.__referencesLeftToResolve == null) {
      throw new Error('__referencesLeftToResolve was unexpectedly null.');
    }

    for (let i = 0; i < this.__referencesLeftToResolve.length; ++i) {
      const ref = this.__referencesLeftToResolve[i];
      this.__delegateToUpperScope(ref);
    }
    this.__referencesLeftToResolve = null;
    return this.upper;
  }
}

export {WithScope};
