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

import type {DeclareNamespace} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class DeclareNamespaceScope extends ScopeBase<
  typeof ScopeType.DeclareNamespace,
  DeclareNamespace,
  Scope,
> {
  declare +type: typeof ScopeType.DeclareNamespace;

  constructor(
    scopeManager: ScopeManager,
    upperScope: DeclareNamespaceScope['upper'],
    block: DeclareNamespaceScope['block'],
  ) {
    super(scopeManager, ScopeType.DeclareNamespace, upperScope, block, false);
  }
}

export {DeclareNamespaceScope};
