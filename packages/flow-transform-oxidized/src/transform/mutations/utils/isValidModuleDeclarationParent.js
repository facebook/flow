/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @noformat
 */

import type {
  ESNode,
  // Used in flow comments
  // eslint-disable-next-line no-unused-vars
  ExportAllDeclaration,
  // Used in flow comments
  // eslint-disable-next-line no-unused-vars
  ExportDefaultDeclaration,
  // Used in flow comments
  // eslint-disable-next-line no-unused-vars
  ExportNamedDeclaration,
  // Used in flow comments
  // eslint-disable-next-line no-unused-vars
  ImportDeclaration,
  ModuleDeclaration,
  Statement,
} from 'flow-estree-oxidized';
import type {DetachedNode} from '../../../detachedNode';

function isModuleDeclaration(node: ESNode) /*: node is (
  | ImportDeclaration
  | ExportNamedDeclaration
  | ExportDefaultDeclaration
  | ExportAllDeclaration
) */ {
  return (
    node.type === 'ImportDeclaration' ||
    node.type === 'ExportNamedDeclaration' ||
    node.type === 'ExportDefaultDeclaration' ||
    node.type === 'ExportAllDeclaration'
  );
}

export function isValidModuleDeclarationParent(
  target: ESNode,
  nodesToInsertOrReplace: $ReadOnlyArray<
    DetachedNode<ModuleDeclaration | Statement>,
  >,
): boolean {
  if (
    target.type === 'Program' ||
    (target.type === 'BlockStatement' && target.parent.type === 'DeclareModule')
  ) {
    return true;
  }

  for (const node of nodesToInsertOrReplace) {
    if (
      !isModuleDeclaration(
        // $FlowExpectedError[incompatible-type]
        (node: ESNode),
      )
    ) {
      continue;
    }

    return false;
  }

  return true;
}
