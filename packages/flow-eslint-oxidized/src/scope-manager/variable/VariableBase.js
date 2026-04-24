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

import type {Identifier} from 'flow-estree-oxidized';
import type {Definition} from '../definition';
import type {Reference} from '../referencer/Reference';
import type {Scope} from '../scope';

import {createIdGenerator} from '../ID';

const generator = createIdGenerator();

interface IVariableBase {
  +$id: number;
  +defs: Array<Definition>;
  // mutable as this can be edited by lint rules
  eslintUsed: boolean;
  +identifiers: Array<Identifier>;
  +name: string;
  +references: Array<Reference>;
  +scope: Scope;
}

class VariableBase implements IVariableBase {
  /**
   * A unique ID for this instance - primarily used to help debugging and testing
   */
  +$id: number = generator();

  /**
   * The array of the definitions of this variable.
   * @public
   */
  +defs: Array<Definition> = [];
  /**
   * True if the variable is considered used for the purposes of `no-unused-vars`, false otherwise.
   * @public
   */
  eslintUsed: boolean = false;
  /**
   * The array of `Identifier` nodes which define this variable.
   * If this variable is redeclared, this array includes two or more nodes.
   * @public
   */
  +identifiers: Array<Identifier> = [];
  /**
   * The variable name, as given in the source code.
   * @public
   */
  +name: string;
  /**
   * List of {@link Reference} of this variable (excluding parameter entries)  in its defining scope and all nested scopes.
   * For defining occurrences only see {@link Variable#defs}.
   * @public
   */
  +references: Array<Reference> = [];
  /**
   * Reference to the enclosing Scope.
   * @public
   */
  +scope: Scope;

  constructor(name: string, scope: Scope) {
    this.name = name;
    this.scope = scope;
  }
}

export type {IVariableBase};
export {VariableBase};
