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

import type {StaticBlock} from 'flow-estree-oxidized';
import type {ClassScope} from './ClassScope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class ClassStaticBlockScope extends ScopeBase<
  typeof ScopeType.ClassStaticBlock,
  StaticBlock,
  ClassScope,
> {
  declare +type: typeof ScopeType.ClassStaticBlock;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ClassStaticBlockScope['upper'],
    block: ClassStaticBlockScope['block'],
  ) {
    super(scopeManager, ScopeType.ClassStaticBlock, upperScope, block, false);
  }
}

export {ClassStaticBlockScope};
