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

import type {BindingName, ESNode} from 'flow-estree';
import type {DefinitionTypeType} from './DefinitionType';

import {createIdGenerator} from '../ID';

const generator = createIdGenerator();

/* abstract */ class DefinitionBase<
  TType extends DefinitionTypeType,
  TNode extends ESNode,
  TParent extends ESNode | null,
  TName extends ESNode = BindingName,
> {
  /**
   * A unique ID for this instance - primarily used to help debugging and testing
   */
  readonly $id: number = generator();

  /**
   * The type of the definition
   * @public
   */
  readonly type: TType;

  /**
   * The `Identifier` node of this definition
   * @public
   */
  readonly name: TName;

  /**
   * The enclosing node of the name.
   * @public
   */
  readonly node: TNode;

  /**
   * the enclosing statement node of the identifier.
   * @public
   */
  readonly parent: TParent;

  constructor(type: TType, name: TName, node: TNode, parent: TParent) {
    this.type = type;
    this.name = name;
    this.node = node;
    this.parent = parent;
  }

  /**
   * `true` if the variable is valid in a type context, false otherwise
   * @public
   */
  readonly isTypeDefinition: boolean;

  /**
   * `true` if the variable is valid in a value context, false otherwise
   * @public
   */
  readonly isVariableDefinition: boolean;
}

export {DefinitionBase};
