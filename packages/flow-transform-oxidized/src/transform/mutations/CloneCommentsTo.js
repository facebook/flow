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
import type {DetachedNode} from '../../detachedNode';
import type {MutationContext} from '../MutationContext';

import {cloneCommentsToNewNode} from '../comments/comments';

export type CloneCommentsToMutation = $ReadOnly<{
  type: 'cloneCommentsTo',
  target: ESNode | DetachedNode<ESNode>,
  destination: ESNode | DetachedNode<ESNode>,
}>;

export function createCloneCommentsToMutation(
  target: CloneCommentsToMutation['target'],
  destination: CloneCommentsToMutation['destination'],
): CloneCommentsToMutation {
  return {
    type: 'cloneCommentsTo',
    target,
    destination,
  };
}

export function performCloneCommentsToMutation(
  _mutationContext: MutationContext,
  mutation: CloneCommentsToMutation,
): null {
  cloneCommentsToNewNode(mutation.target, mutation.destination);
  return null;
}
