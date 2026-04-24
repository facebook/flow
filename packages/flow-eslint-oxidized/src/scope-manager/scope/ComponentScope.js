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

import type {ComponentDeclaration} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';
import type {Reference} from '../referencer/Reference';
import type {Variable} from '../variable';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class ComponentScope extends ScopeBase<
  typeof ScopeType.Component,
  ComponentDeclaration,
  Scope,
> {
  declare +type: typeof ScopeType.Component;

  constructor(
    scopeManager: ScopeManager,
    upperScope: ComponentScope['upper'],
    block: ComponentScope['block'],
  ) {
    super(scopeManager, ScopeType.Component, upperScope, block, false);
  }

  // References in default parameters isn't resolved to variables which are in their body.
  //     const x = 1
  //     component Foo(a = x) { // This `x` is resolved to the `x` in the outer scope.
  //         const x = 2
  //         console.log(a)
  //     }
  __isValidResolution(ref: Reference, variable: Variable): boolean {
    const bodyStart = this.block.body.range[0] ?? -1;

    // It's invalid resolution in the following case:
    return !(
      (
        variable.scope === this &&
        ref.identifier.range[0] < bodyStart && // the reference is in the parameter part.
        variable.defs.every(d => d.name.range[0] >= bodyStart)
      ) // the variable is in the body.
    );
  }
}

export {ComponentScope};
