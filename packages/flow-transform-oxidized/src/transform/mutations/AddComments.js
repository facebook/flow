/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {Comment, ESNode} from 'flow-estree-oxidized';
import type {DetachedNode} from '../../detachedNode';
import type {MutationContext} from '../MutationContext';
import type {CommentPlacement} from '../comments/comments';

import {addComment, cloneComment} from '../comments/comments';

export type AddCommentsMutation = $ReadOnly<{
  type: 'addComments',
  comments: $ReadOnlyArray<{comment: Comment, placement: CommentPlacement}>,
  node: ESNode | DetachedNode<ESNode>,
}>;

export function createAddCommentsMutation(
  node: AddCommentsMutation['node'],
  comments: AddCommentsMutation['comments'],
): ?AddCommentsMutation {
  if (comments.length === 0) {
    return null;
  }

  return {
    type: 'addComments',
    comments,
    node,
  };
}

export function performAddCommentsMutation(
  mutationContext: MutationContext,
  mutation: AddCommentsMutation,
): null {
  for (const {comment: originalComment, placement} of mutation.comments) {
    const comment = cloneComment(originalComment);
    mutationContext.appendCommentToSource(comment, placement);
    addComment(mutation.node, comment, placement);
  }

  return null;
}
