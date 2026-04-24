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
  AFunction,
  Identifier,
  ComponentDeclaration,
  DeclareComponent,
  DeclareHook,
  HookDeclaration,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class ParameterDefinition extends DefinitionBase<
  typeof DefinitionType.Parameter,
  | AFunction
  | ComponentDeclaration
  | DeclareComponent
  | HookDeclaration
  | DeclareHook,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.Parameter;

  /**
   * Whether the parameter definition is a part of a rest parameter.
   */
  +rest: boolean;

  constructor(
    name: Identifier,
    node: ParameterDefinition['node'],
    rest: boolean,
  ) {
    super(DefinitionType.Parameter, name, node, null);
    this.rest = rest;
  }

  +isTypeDefinition: boolean = false;
  +isVariableDefinition: boolean = true;
}

export {ParameterDefinition};
