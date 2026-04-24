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
  ExportAllDeclaration,
  ExportDefaultDeclaration,
  ExportNamedDeclaration,
  ExportSpecifier,
  Identifier,
} from 'flow-estree-oxidized';
import type {Referencer} from './Referencer';

import {Visitor} from './Visitor';

type ExportNode =
  | ExportAllDeclaration
  | ExportDefaultDeclaration
  | ExportNamedDeclaration;

class ExportVisitor extends Visitor {
  +_referencer: Referencer;
  +_exportNode: ExportNode;

  constructor(node: ExportNode, referencer: Referencer) {
    super(referencer);
    this._exportNode = node;
    this._referencer = referencer;
  }

  static visit(referencer: Referencer, node: ExportNode): void {
    const exportReferencer = new ExportVisitor(node, referencer);
    exportReferencer.visit(node);
  }

  Identifier(node: Identifier): void {
    if (this._exportNode.exportKind === 'type') {
      // export type { T };
      // type exports can only reference types
      this._referencer.currentScope().referenceType(node);
    } else {
      this._referencer.currentScope().referenceDualValueType(node);
    }
  }

  ExportDefaultDeclaration(node: ExportDefaultDeclaration): void {
    if (node.declaration.type === 'Identifier') {
      // export default A;
      // this could be a type or a variable
      this.visit(node.declaration);
    } else {
      // export const a = 1;
      // export something();
      // etc
      // these not included in the scope of this visitor as they are all guaranteed to be values or declare variables
    }
  }

  ExportNamedDeclaration(node: ExportNamedDeclaration): void {
    if (node.source) {
      // export ... from 'foo';
      // these are external identifiers so there shouldn't be references or defs
      return;
    }

    if (!node.declaration) {
      // export { x };
      this.visitChildren(node);
    } else {
      // export const x = 1;
      // this is not included in the scope of this visitor as it creates a variable
    }
  }

  ExportSpecifier(node: ExportSpecifier): void {
    this.visit(node.local);
  }
}

export {ExportVisitor};
