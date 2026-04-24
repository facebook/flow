/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {ESNode} from 'flow-estree-oxidized';
import type {MutationContext} from '../MutationContext';
import type {DetachedNode} from '../../detachedNode';

import {
  getVisitorKeys,
  isNode,
  astArrayMutationHelpers,
} from 'flow-parser-oxidized';
import {moveCommentsToNewNode} from '../comments/comments';
import {InvalidReplacementError} from '../Errors';
import {getOriginalNode} from '../../detachedNode';

export type ReplaceNodeMutation = $ReadOnly<{
  type: 'replaceNode',
  target: ESNode,
  nodeToReplaceWith: DetachedNode<ESNode>,
  keepComments: boolean,
}>;

export function createReplaceNodeMutation(
  target: ReplaceNodeMutation['target'],
  nodeToReplaceWith: ReplaceNodeMutation['nodeToReplaceWith'],
  options?: $ReadOnly<{keepComments?: boolean}>,
): ReplaceNodeMutation {
  return {
    type: 'replaceNode',
    target,
    nodeToReplaceWith,
    keepComments: options?.keepComments ?? false,
  };
}

export function performReplaceNodeMutation(
  mutationContext: MutationContext,
  mutation: ReplaceNodeMutation,
): ESNode {
  const replacementParent = getParentKey(mutation.target);

  mutationContext.markDeletion(mutation.target);
  mutationContext.markMutation(replacementParent.parent, replacementParent.key);

  // NOTE: currently this mutation assumes you're doing the right thing.
  // it does no runtime checks and provides no guarantees about the
  // correctness of the resulting code.
  // TODO: maybe add some runtime checks based on codegenned predicates?

  if (replacementParent.type === 'array') {
    const parent: interface {
      [string]: $ReadOnlyArray<DetachedNode<ESNode>>,
    } = replacementParent.parent;
    parent[replacementParent.key] = astArrayMutationHelpers.replaceInArray(
      parent[replacementParent.key],
      replacementParent.targetIndex,
      [mutation.nodeToReplaceWith],
    );
  } else {
    (replacementParent.parent: interface {[string]: mixed})[
      replacementParent.key
    ] = mutation.nodeToReplaceWith;
  }

  if (mutation.keepComments) {
    moveCommentsToNewNode(mutation.target, mutation.nodeToReplaceWith);
  }

  return replacementParent.parent;
}

function getParentKey(target: ESNode): $ReadOnly<
  | {
      type: 'single',
      parent: ESNode,
      key: string,
    }
  | {
      type: 'array',
      parent: ESNode,
      key: string,
      targetIndex: number,
    },
> {
  const parent = target.parent;
  for (const key of getVisitorKeys(parent)) {
    const child = (parent: $FlowFixMe)[key];
    if (isNode(child)) {
      // $FlowFixMe[invalid-compare]
      if (child === target) {
        return {type: 'single', parent, key};
      }
    } else if (Array.isArray(child)) {
      for (let i = 0; i < child.length; i += 1) {
        const current = child[i];
        const originalNode = getOriginalNode(current);
        if (current === target || originalNode === target) {
          return {type: 'array', parent, key, targetIndex: i};
        }
      }
    }
  }

  // this shouldn't happen ever
  throw new InvalidReplacementError(
    `Expected to find the ${target.type} as a direct child of the ${parent.type}.`,
  );
}
