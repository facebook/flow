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

import type {Program} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {Reference} from '../referencer/Reference';
import type {ScopeManager} from '../ScopeManager';
import type {Variable} from '../variable';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';
import {ImplicitGlobalVariableDefinition} from '../definition/ImplicitGlobalVariableDefinition';

class GlobalScope extends ScopeBase<
  typeof ScopeType.Global,
  Program,
  /**
   * The global scope has no parent.
   */
  null,
> {
  declare +type: typeof ScopeType.Global;

  // note this is accessed in used in the legacy eslint-scope tests, so it can't be true private
  +__implicit: {
    +set: Map<string, Variable>,
    +variables: Array<Variable>,
    /**
     * List of {@link Reference}s that are left to be resolved (i.e. which
     * need to be linked to the variable they refer to).
     */
    referencesLeftToResolve: Array<Reference>,
  };

  constructor(scopeManager: ScopeManager, block: GlobalScope['block']) {
    super(scopeManager, ScopeType.Global, null, block, false);
    this.__implicit = {
      set: new Map<string, Variable>(),
      variables: [],
      referencesLeftToResolve: [],
    };
  }

  close(scopeManager: ScopeManager): Scope | null {
    if (this.__referencesLeftToResolve == null) {
      throw new Error('__referencesLeftToResolve was unexpectedly null.');
    }

    for (const ref of this.__referencesLeftToResolve) {
      const info = ref.maybeImplicitGlobal;
      if (info && !this.set.has(ref.identifier.name)) {
        // create an implicit global variable from assignment expression
        const node = info.pattern;
        if (node && node.type === 'Identifier') {
          this.__defineVariable(
            node.name,
            this.__implicit.set,
            this.__implicit.variables,
            node,
            new ImplicitGlobalVariableDefinition(info.pattern, info.node),
          );
        }
      }
    }

    // $FlowFixMe[incompatible-type] We know this value is an array at this point.
    this.__implicit.referencesLeftToResolve = this.__referencesLeftToResolve;
    return super.close(scopeManager);
  }
}

export {GlobalScope};
