/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {Comment, Program} from 'flow-estree-oxidized';

import {SimpleTraverser, SimpleTraverserBreak} from 'flow-parser-oxidized';
import {getCommentsForNode, setCommentsOnNode} from '../comments/comments';

export type RemoveCommentMutation = $ReadOnly<{
  type: 'removeComment',
  comment: Comment,
}>;

export function createRemoveCommentMutation(
  comment: RemoveCommentMutation['comment'],
): RemoveCommentMutation {
  return {
    type: 'removeComment',
    comment,
  };
}

export function performRemoveCommentMutations(
  ast: Program,
  mutations: $ReadOnlyArray<RemoveCommentMutation>,
): void {
  if (mutations.length === 0) {
    return;
  }

  const commentsToRemove = new Set(mutations.map(m => m.comment));

  SimpleTraverser.traverse(ast, {
    enter(node) {
      if (node === ast) {
        return;
      }

      const nodeCommentsSet = new Set(getCommentsForNode(node));
      if (nodeCommentsSet.size === 0) {
        return;
      }

      const matchedComments = intersectSets(commentsToRemove, nodeCommentsSet);
      for (const comment of matchedComments) {
        commentsToRemove.delete(comment);
        nodeCommentsSet.delete(comment);
      }
      setCommentsOnNode(node, Array.from(nodeCommentsSet));

      if (commentsToRemove.size === 0) {
        // no more comments to process - so we can exit traversal
        throw SimpleTraverserBreak;
      }
    },
    leave() {},
  });
}

function intersectSets<T>(
  first: $ReadOnlySet<T>,
  other: $ReadOnlySet<T>,
): Set<T> {
  const ret = new Set<T>();
  for (const value of first) {
    if (other.has(value)) {
      ret.add(value);
    }
  }
  return ret;
}
