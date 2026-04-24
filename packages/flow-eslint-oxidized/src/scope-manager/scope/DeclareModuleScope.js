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

import type {DeclareModule} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class DeclareModuleScope extends ScopeBase<
  typeof ScopeType.DeclareModule,
  DeclareModule,
  Scope,
> {
  declare +type: typeof ScopeType.DeclareModule;

  constructor(
    scopeManager: ScopeManager,
    upperScope: DeclareModuleScope['upper'],
    block: DeclareModuleScope['block'],
  ) {
    super(scopeManager, ScopeType.DeclareModule, upperScope, block, false);
  }
}

export {DeclareModuleScope};
