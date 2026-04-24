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

import type {ClassDeclaration, ClassExpression} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class ClassScope extends ScopeBase<
  typeof ScopeType.Class,
  ClassDeclaration | ClassExpression,
  Scope,
> {
  declare +type: typeof ScopeType.Class;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ClassScope['upper'],
    block: ClassScope['block'],
  ) {
    super(scopeManager, ScopeType.Class, upperScope, block, false);
  }
}

export {ClassScope};
