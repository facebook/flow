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
import {isValidModuleDeclarationParent} from './utils/isValidModuleDeclarationParent';
import {InvalidInsertionError} from '../Errors';
import * as t from '../../generated/node-types';

export type InsertStatementMutation = $ReadOnly<{
  type: 'insertStatement',
  side: 'before' | 'after',
  target: ModuleDeclaration | Statement,
  nodesToInsert: $ReadOnlyArray<DetachedNode<Statement | ModuleDeclaration>>,
}>;

export function createInsertStatementMutation(
  side: InsertStatementMutation['side'],
  target: InsertStatementMutation['target'],
  nodesToInsert: InsertStatementMutation['nodesToInsert'],
): ?InsertStatementMutation {
  if (nodesToInsert.length === 0) {
    return null;
  }

  return {
    type: 'insertStatement',
    side,
    target,
    nodesToInsert,
  };
}

export function performInsertStatementMutation(
  mutationContext: MutationContext,
  mutation: InsertStatementMutation,
): ESNode {
  mutationContext.assertNotDeleted(
    mutation.target,
    `Attempted to insert ${mutation.side} a deleted ${mutation.target.type} node. This likely means that you attempted to mutate around the target after it was deleted/replaced.`,
  );

  const insertionParent = getStatementParent(mutation.target);

  // enforce that if we are inserting module declarations - they are being inserted in a valid location
  if (
    !isValidModuleDeclarationParent(
      insertionParent.parent,
      mutation.nodesToInsert,
    )
  ) {
    throw new InvalidInsertionError(
      `import/export cannot be inserted into a ${insertionParent.parent.type}.`,
    );
  }

  mutationContext.markMutation(insertionParent.parent, insertionParent.key);

  if (insertionParent.type === 'array') {
    const parent: interface {
      [string]: $ReadOnlyArray<DetachedNode<Statement | ModuleDeclaration>>,
    } = insertionParent.parent;
    switch (mutation.side) {
      case 'before': {
        parent[insertionParent.key] = astArrayMutationHelpers.insertInArray(
          parent[insertionParent.key],
          insertionParent.targetIndex,
          mutation.nodesToInsert,
        );
        break;
      }

      case 'after': {
        parent[insertionParent.key] = astArrayMutationHelpers.insertInArray(
          parent[insertionParent.key],
          insertionParent.targetIndex + 1,
          mutation.nodesToInsert,
        );
        break;
      }
    }

    return insertionParent.parent;
  }

  const statementsToInsert =
    // $FlowExpectedError[incompatible-type] -- this is enforced by isValidModuleDeclarationParent above
    (mutation.nodesToInsert: $ReadOnlyArray<DetachedNode<Statement>>);

  const {parent, key} = insertionParent;

  // $FlowExpectedError[prop-missing]
  const statementToWrap = parent[key];
  // we need to wrap this key in a BlockStatement so we can insert the new statement
  const blockStatement = t.BlockStatement({
    body:
      mutation.side === 'before'
        ? [...statementsToInsert, statementToWrap]
        : [statementToWrap, ...statementsToInsert],
    parent: insertionParent.parent,
  });

  (insertionParent.parent: interface {[string]: mixed})[insertionParent.key] =
    blockStatement;
  statementToWrap.parent = blockStatement;

  return insertionParent.parent;
}
