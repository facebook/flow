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

import type {Expression} from 'flow-estree-oxidized';
import type {ClassScope} from './ClassScope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class ClassFieldInitializerScope extends ScopeBase<
  typeof ScopeType.ClassFieldInitializer,
  // the value expression itself is the block
  Expression,
  ClassScope,
> {
  declare +type: typeof ScopeType.ClassFieldInitializer;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ClassFieldInitializerScope['upper'],
    block: ClassFieldInitializerScope['block'],
  ) {
    super(
      scopeManager,
      ScopeType.ClassFieldInitializer,
      upperScope,
      block,
      false,
    );
  }
}

export {ClassFieldInitializerScope};
