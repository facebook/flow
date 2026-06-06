/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ESNode, ModuleDeclaration, Statement} from 'flow-estree';
import type {MutationContext} from '../MutationContext';
import type {DetachedNode} from '../../detachedNode';

import {astArrayMutationHelpers} from 'flow-parser/oxidized';
import {getStatementParent} from './utils/getStatementParent';
import {isValidModuleDeclarationParent} from './utils/isValidModuleDeclarationParent';
import {moveCommentsToNewNode} from '../comments/comments';
import {InvalidReplacementError} from '../Errors';
import * as t from '../../generated/node-types';

export type ReplaceStatementWithManyMutationNodes =
  | ModuleDeclaration
  | Statement;
export type ReplaceStatementWithManyMutation = Readonly<{
  type: 'replaceStatementWithMany',
  target: ReplaceStatementWithManyMutationNodes,
  nodesToReplaceWith: ReadonlyArray<
    DetachedNode<ReplaceStatementWithManyMutationNodes>,
  >,
  keepComments: boolean,
}>;

export function createReplaceStatementWithManyMutation(
  target: ReplaceStatementWithManyMutation['target'],
  nodesToReplaceWith: ReplaceStatementWithManyMutation['nodesToReplaceWith'],
  options?: Readonly<{keepComments?: boolean}>,
): ?ReplaceStatementWithManyMutation {
  if (nodesToReplaceWith.length === 0) {
    return null;
  }

  return {
    type: 'replaceStatementWithMany',
    target,
    nodesToReplaceWith,
    keepComments: options?.keepComments ?? false,
  };
}

export function performReplaceStatementWithManyMutation(
  mutationContext: MutationContext,
  mutation: ReplaceStatementWithManyMutation,
): ESNode {
  const replacementParent = getStatementParent(mutation.target);

  // enforce that if we are replacing with module declarations - they are being inserted in a valid location
  if (
    !isValidModuleDeclarationParent(
      replacementParent.parent,
      mutation.nodesToReplaceWith,
    )
  ) {
    throw new InvalidReplacementError(
      `import/export cannot be replaced into a ${replacementParent.parent.type}.`,
    );
  }

  mutationContext.markDeletion(mutation.target);
  mutationContext.markMutation(replacementParent.parent, replacementParent.key);

  if (mutation.keepComments) {
    // attach comments to the very first replacement node
    moveCommentsToNewNode(mutation.target, mutation.nodesToReplaceWith[0]);
  }

  if (replacementParent.type === 'array') {
    const parent: interface {
      [string]: ReadonlyArray<DetachedNode<Statement | ModuleDeclaration>>,
    } = replacementParent.parent;
    parent[replacementParent.key] = astArrayMutationHelpers.replaceInArray(
      parent[replacementParent.key],
      replacementParent.targetIndex,
      mutation.nodesToReplaceWith,
    );

    return replacementParent.parent;
  }

  const statementsToReplaceWith =
    // $FlowExpectedError[incompatible-type] -- this is enforced by isValidModuleDeclarationParent above
    mutation.nodesToReplaceWith as ReadonlyArray<DetachedNode<Statement>>;

  // we need to wrap the nodes in a BlockStatement as before there was only 1 node
  const blockStatement = t.BlockStatement({
    body: statementsToReplaceWith,
    parent: replacementParent.parent,
  });

  (replacementParent.parent as interface {[string]: unknown})[
    replacementParent.key
  ] = blockStatement;

  return replacementParent.parent;
}
