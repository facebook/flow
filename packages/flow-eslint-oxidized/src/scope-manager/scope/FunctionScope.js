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

import type {
  ArrowFunctionExpression,
  FunctionDeclaration,
  FunctionExpression,
  Program,
} from 'flow-estree-oxidized';
import type {Scope} from './Scope';
import type {ScopeManager} from '../ScopeManager';
import type {Reference} from '../referencer/Reference';
import type {Variable} from '../variable';

import {ScopeBase} from './ScopeBase';
import {ScopeType} from './ScopeType';

class FunctionScope extends ScopeBase<
  typeof ScopeType.Function,
  ArrowFunctionExpression | FunctionDeclaration | FunctionExpression | Program,
  Scope,
> {
  declare +type: typeof ScopeType.Function;

  constructor(
    scopeManager: ScopeManager,
    upperScope: FunctionScope['upper'],
    block: FunctionScope['block'],
    isMethodDefinition: boolean,
  ) {
    super(
      scopeManager,
      ScopeType.Function,
      upperScope,
      block,
      isMethodDefinition,
    );

    // section 9.2.13, FunctionDeclarationInstantiation.
    // NOTE Arrow functions never have an arguments objects.
    if (this.block.type !== 'ArrowFunctionExpression') {
      this.__defineVariable('arguments', this.set, this.variables, null, null);
    }
  }

  // References in default parameters isn't resolved to variables which are in their function body.
  //     const x = 1
  //     function f(a = x) { // This `x` is resolved to the `x` in the outer scope.
  //         const x = 2
  //         console.log(a)
  //     }
  __isValidResolution(ref: Reference, variable: Variable): boolean {
    // If `options.gloablReturn` is true, `this.block` becomes a Program node.
    if (this.block.type === 'Program') {
      return true;
    }

    const bodyStart = this.block.body?.range[0] ?? -1;

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

export {FunctionScope};
