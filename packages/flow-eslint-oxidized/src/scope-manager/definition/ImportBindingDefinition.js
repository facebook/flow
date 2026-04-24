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
  ImportSpecifier,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportDeclaration,
  Identifier,
} from 'flow-estree-oxidized';

import {DefinitionType} from './DefinitionType';
import {DefinitionBase} from './DefinitionBase';

class ImportBindingDefinition extends DefinitionBase<
  typeof DefinitionType.ImportBinding,
  ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier,
  ImportDeclaration,
  Identifier,
> {
  declare +type: typeof DefinitionType.ImportBinding;

  constructor(
    name: Identifier,
    node: ImportBindingDefinition['node'],
    decl: ImportDeclaration,
  ) {
    super(DefinitionType.ImportBinding, name, node, decl);
    switch (node.type) {
      case 'ImportSpecifier':
        if (
          node.importKind === 'type' ||
          node.importKind === 'typeof' ||
          decl.importKind === 'type' ||
          decl.importKind === 'typeof'
        ) {
          this.isVariableDefinition = false;
        } else {
          this.isVariableDefinition = true;
        }
        break;

      case 'ImportDefaultSpecifier':
        if (decl.importKind === 'type' || decl.importKind === 'typeof') {
          this.isVariableDefinition = false;
        } else {
          this.isVariableDefinition = true;
        }
        break;

      case 'ImportNamespaceSpecifier':
        this.isVariableDefinition = true;
        break;
    }
  }

  // all imports are treated as "type" definitions regardless of whether or
  // not they have a "type" kind. You can import classes and enums as values
  // and use them in type locations. Additionally namespace imports can be
  // used in a type location (eg React.MixedElement).
  +isTypeDefinition: boolean = true;
  +isVariableDefinition: boolean;
}

export {ImportBindingDefinition};
