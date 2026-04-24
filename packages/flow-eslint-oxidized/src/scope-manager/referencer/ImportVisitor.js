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
  Identifier,
  ImportDeclaration,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportSpecifier,
} from 'flow-estree-oxidized';
import type {Referencer} from './Referencer';

import {ImportBindingDefinition} from '../definition';
import {Visitor} from './Visitor';

class ImportVisitor extends Visitor {
  +_declaration: ImportDeclaration;
  +_referencer: Referencer;

  constructor(declaration: ImportDeclaration, referencer: Referencer) {
    super(referencer);
    this._declaration = declaration;
    this._referencer = referencer;
  }

  static visit(referencer: Referencer, declaration: ImportDeclaration): void {
    const importReferencer = new ImportVisitor(declaration, referencer);
    importReferencer.visit(declaration);
  }

  visitImport(
    id: Identifier,
    specifier:
      | ImportDefaultSpecifier
      | ImportNamespaceSpecifier
      | ImportSpecifier,
  ): void {
    this._referencer
      .currentScope()
      .defineIdentifier(
        id,
        new ImportBindingDefinition(id, specifier, this._declaration),
      );
  }

  ImportNamespaceSpecifier(node: ImportNamespaceSpecifier): void {
    const local = node.local;
    this.visitImport(local, node);
  }

  ImportDefaultSpecifier(node: ImportDefaultSpecifier): void {
    const local = node.local;
    this.visitImport(local, node);
  }

  ImportSpecifier(node: ImportSpecifier): void {
    const local = node.local;
    this.visitImport(local, node);
  }
}

export {ImportVisitor};
