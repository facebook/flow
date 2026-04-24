/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

// flowlint unsafe-getters-setters:off

'use strict';

import type {IVariableBase} from './VariableBase';

import {VariableBase} from './VariableBase';

interface IVariable extends IVariableBase {
  +isTypeVariable: boolean;
  +isValueVariable: boolean;
}

/**
 * A Variable represents a locally scoped identifier. These include arguments to functions.
 */
class Variable extends VariableBase implements IVariable {
  /**
   * `true` if the variable is valid in a type context, false otherwise
   * @public
   */
  get isTypeVariable(): boolean {
    if (this.defs.length === 0) {
      // we don't statically know whether this is a type or a value
      return true;
    }

    return this.defs.some(def => def.isTypeDefinition);
  }

  /**
   * `true` if the variable is valid in a value context, false otherwise
   * @public
   */
  get isValueVariable(): boolean {
    if (this.defs.length === 0) {
      // we don't statically know whether this is a type or a value
      return true;
    }

    return this.defs.some(def => def.isVariableDefinition);
  }
}

export type {IVariable};
export {Variable};
