/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ESNode, ModuleDeclaration, Statement} from 'flow-estree-oxidized';
import type {MutationContext} from '../MutationContext';
import type {DetachedNode} from '../../detachedNode';

import {astArrayMutationHelpers} from 'flow-parser-oxidized';
import {getStatementParent} from './utils/getStatementParent';
import * as t from '../../generated/node-types';

export type RemoveStatementMutation = $ReadOnly<{
  type: 'removeStatement',
  node: ModuleDeclaration | Statement,
}>;

export function createRemoveStatementMutation(
  node: RemoveStatementMutation['node'],
): RemoveStatementMutation {
  return {
    type: 'removeStatement',
    node,
  };
}

export function performRemoveStatementMutation(
  mutationContext: MutationContext,
  mutation: RemoveStatementMutation,
): ESNode {
  const removalParent = getStatementParent(mutation.node);

  mutationContext.markDeletion(mutation.node);
  mutationContext.markMutation(removalParent.parent, removalParent.key);

  if (removalParent.type === 'array') {
    const parent: interface {
      [string]: $ReadOnlyArray<DetachedNode<Statement | ModuleDeclaration>>,
    } = removalParent.parent;
    parent[removalParent.key] = astArrayMutationHelpers.removeFromArray(
      parent[removalParent.key],
      removalParent.targetIndex,
    );
  } else {
    // The parent has a 1:1 relationship on this key, so we can't just
    // remove the node. Instead we replace it with an empty block statement.
    // We COULD throw an error here and make the codemodder write a stricter
    // codemod - but we decided to add this bit of magic to make it easier
    // to write codemods.
    // Worst case it creates some dead code that can be easily detected
    // and cleaned up later.
    const blockStatement = t.BlockStatement({
      body: [],
      parent: removalParent.parent,
    });

    (removalParent.parent: interface {[string]: mixed})[removalParent.key] =
      blockStatement;
  }

  return removalParent.parent;
}
