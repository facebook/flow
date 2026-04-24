/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {CommentPlacement} from './comments/comments';
import type {Comment, ESNode} from 'flow-estree-oxidized';

import {NodeIsDeletedError, NodeIsMutatedError} from './Errors';
import {appendCommentToSource} from './comments/comments';

export class MutationContext {
  +_deletedNodes: Set<ESNode> = new Set();
  +_mutatedKeys: Map<ESNode, Set<string>> = new Map();
  // TODO - do we care about this? Arrays are pretty safe to concurrently mutate
  +_mutatedArrays: Map<ESNode, Set<string>> = new Map();

  code: string;

  constructor(code: string) {
    this.code = code;
  }

  /**
   * Marks a node and its entire subtree as deleted.
   */
  markDeletion(node: ESNode): void {
    this._deletedNodes.add(node);
  }

  /**
   * Marks the key of the node as having been mutated.
   */
  markMutation(node: ESNode, key: string): void {
    this.assertNotDeleted(
      node,
      `Attempted to mutate a \`${node.type}.${key}\` on a deleted node.`,
    );
    this.assertNotMutated(
      node,
      key,
      `Attempted to mutate a \`${node.type}.${key}\` when it has already been mutated.`,
    );

    const map = Array.isArray(
      // $FlowExpectedError[prop-missing]
      node[key],
    )
      ? this._mutatedArrays
      : this._mutatedKeys;

    map.set(node, map.get(node)?.add(key) ?? new Set([key]));
  }

  /**
   * Throws if the node has been deleted
   */
  assertNotDeleted(node: ESNode, message: string): void {
    if (this._deletedNodes.has(node)) {
      throw new NodeIsDeletedError(message);
    }
  }

  /**
   * Throws if the key of the node has been mutated
   */
  assertNotMutated(node: ESNode, key: string, message: string): void {
    if (this._mutatedKeys.get(node)?.has(key) === true) {
      throw new NodeIsMutatedError(message);
    }
  }

  appendCommentToSource(comment: Comment, placement: CommentPlacement): void {
    this.code = appendCommentToSource(this.code, comment, placement);
  }
}
