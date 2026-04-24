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

import type {TypeParameter, Identifier} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class TypeParameterDefinition extends DefinitionBase<
  typeof DefinitionType.TypeParameter,
  TypeParameter,
  null,
  Identifier,
> {
  declare +type: typeof DefinitionType.TypeParameter;

  constructor(node: TypeParameterDefinition['node']) {
    // The ScopeManager API expects an Identifier node that can be referenced
    // for each definition. TypeParameter nodes do not actually contain an
    // Identifier node, so we create a fake one with the correct name,
    // location, and parent so that it is still usable with the ScopeManager.
    const id: Identifier = {
      type: 'Identifier',
      loc: node.loc,
      name: node.name,
      parent: node,
      range: node.range,
      optional: false,
      typeAnnotation: null,
    };
    super(DefinitionType.TypeParameter, id, node, null);
  }

  +isTypeDefinition: boolean = true;
  +isVariableDefinition: boolean = false;
}

export {TypeParameterDefinition};
