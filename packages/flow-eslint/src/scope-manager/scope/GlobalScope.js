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

import type {Program} from 'flow-estree';
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
    variables: Array<Variable>,
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

  addVariables(names: ReadonlyArray<string>): void {
    for (const name of names) {
      this.__defineVariable(name, this.set, this.variables, null, null);
      this.__implicit.set.delete(name);
    }

    const nameSet = new Set(names);

    for (const reference of this.through) {
      if (nameSet.has(reference.identifier.name)) {
        const variable = this.set.get(reference.identifier.name);
        if (!variable) {
          throw new Error(
            `Expected variable with name "${reference.identifier.name}" to be defined.`,
          );
        }
        reference.resolved = variable;
        variable.references.push(reference);
      }
    }

    this.through = this.through.filter(
      reference => !nameSet.has(reference.identifier.name),
    );
    this.__implicit.variables = this.__implicit.variables.filter(
      variable => !nameSet.has(variable.name),
    );
    this.__implicit.referencesLeftToResolve =
      this.__implicit.referencesLeftToResolve.filter(
        reference => !nameSet.has(reference.identifier.name),
      );
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

    super.close(scopeManager);
    this.__implicit.referencesLeftToResolve = [...this.through];

    return null;
  }
}

export {GlobalScope};
