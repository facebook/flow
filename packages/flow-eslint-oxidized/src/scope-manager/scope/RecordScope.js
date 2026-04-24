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

import type {RecordDeclaration} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class RecordScope extends ScopeBase<
  typeof ScopeType.Record,
  RecordDeclaration,
  Scope,
> {
  declare +type: typeof ScopeType.Record;

  constructor(
    scopeManager: ScopeManager,
    upperScope: RecordScope['upper'],
    block: RecordScope['block'],
  ) {
    super(scopeManager, ScopeType.Record, upperScope, block, false);
  }
}

export {RecordScope};
